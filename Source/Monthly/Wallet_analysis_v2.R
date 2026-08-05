library(tidyverse)
library(dplyr)
library(readxl)
library(stringr)
library(lubridate)

# Direct entities are declared before the general LCO functions are called so
# that their data is excluded from the general LCO outputs.
direct_cus = c(
  'MD0440',
  'MBDML',
  'MD0479',
  'MD0478',
  "MD0493",
  "MD0495",
  "MD0512",
  "MD0515",
  "MDOGH",
  "MDHRO",
  "MD0521",
  "MDZTPN"
)

# Helper function to get the correct 7-zip executable based on OS
get_7zip_path <- function() {
  os <- Sys.info()["sysname"]

  if (os == "Windows") {
    # Use standard 7-Zip installation path
    path <- "C:\\Program Files\\7-Zip\\7z.exe"
    if (!file.exists(path)) {
      # Try alternative path (32-bit on 64-bit Windows)
      path <- "C:\\Program Files (x86)\\7-Zip\\7z.exe"
    }
    if (!file.exists(path)) {
      stop("7-Zip not found. Please install 7-Zip or add it to PATH.")
    }
    return(path)

  } else if (os == "Linux") {
    # Verify 7z is available
    path <- "7z"
    if (system2("which", args = "7z", stdout = FALSE, stderr = FALSE) != 0) {
      stop("7-Zip not found. Please install it using: sudo apt install p7zip-full")
    }
    return(path)

  } else if (os == "Darwin") {
    path <- "7z"
    if (file.exists("/usr/local/bin/7z")) {
      path <- "/usr/local/bin/7z"
    } else if (file.exists("/opt/homebrew/bin/7z")) {
      path <- "/opt/homebrew/bin/7z"
    }
    if (system2("which", args = "7z", stdout = FALSE, stderr = FALSE) != 0) {
      stop("7-Zip not found. Please install it using: brew install p7zip")
    }
    return(path)

  } else {
    stop(paste("Unsupported operating system:", os))
  }
}

parse_bill_date <- function(value) {
  # The export uses DD/MM/YYYY. For example, 05/08/2026 to 03/09/2026 is a
  # 30-day service interval, so it must not be parsed as an American date.
  dmy_hms(as.character(value), quiet = TRUE)
}

safe_min_datetime <- function(value) {
  value <- value[!is.na(value)]
  if (length(value) == 0) {
    return(as.POSIXct(NA))
  }
  min(value)
}

# When service intervals overlap for the same customer, retain the entry with
# the latest start date. Entries beginning after the previous end date remain.
resolve_customer_overlaps <- function(direct_bill) {
  direct_bill %>%
    group_by(Entity.Code, Customer.Nbr) %>%
    group_modify(~ {
      entries <- .x %>%
        arrange(Bill.Charge.Start.Date, Bill.Charge.End.Date, .source_order)

      if (nrow(entries) <= 1) {
        return(entries)
      }

      kept <- entries[1, , drop = FALSE]
      if (nrow(entries) > 1) {
        for (entry_number in 2:nrow(entries)) {
          current <- entries[entry_number, , drop = FALSE]
          previous <- kept[nrow(kept), , drop = FALSE]

          overlaps <- !is.na(current$Bill.Charge.Start.Date) &&
            !is.na(previous$Bill.Charge.End.Date) &&
            current$Bill.Charge.Start.Date <= previous$Bill.Charge.End.Date

          if (overlaps) {
            # The data is ordered by start date, so the current entry is the
            # later-starting entry and replaces the older overlapping entry.
            kept[nrow(kept), ] <- current
          } else {
            kept <- bind_rows(kept, current)
          }
        }
      }
      kept
    }) %>%
    ungroup()
}

build_direct_bill_detail <- function(wallet_in, direct_entities) {
  direct_rows <- wallet_in %>%
    filter(
      Entity.Code %in% direct_entities,
      !is.na(Customer.Nbr),
      Customer.Nbr != ""
    ) %>%
    mutate(
      .source_order = row_number(),
      .raw_amount = as.numeric(Amount.Debit),
      Amount.Debit = round(.raw_amount, digits = 2),
      Bill.Charge.Start.Date = parse_bill_date(Bill.Charge.Start.Date),
      Bill.Charge.End.Date = parse_bill_date(Bill.Charge.End.Date),
      # Missing amounts sort behind real amounts when choosing the source row.
      .amount_for_selection = if_else(
        is.na(.raw_amount),
        -Inf,
        .raw_amount
      )
    ) %>%
    arrange(Entity.Code, Customer.Nbr, Contract.Number,
            Bill.Charge.End.Date, desc(.amount_for_selection), .source_order)

  consolidated <- direct_rows %>%
    group_by(Entity.Code, Customer.Nbr, Contract.Number, Bill.Charge.End.Date) %>%
    arrange(desc(.amount_for_selection), .source_order, .by_group = TRUE) %>%
    summarise(
      Customer.Name = first(Customer.Name),
      Unique.Id = first(Unique.Id),
      Entity.Name = first(Entity.Name),
      Address = first(Address),
      Mobile = first(Mobile),
      Plan.Details = {
        tied_plans = Plan.Details[
          !is.na(Plan.Details) & nzchar(Plan.Details) &
            .amount_for_selection == max(.amount_for_selection)
        ]
        if (length(tied_plans) == 0) {
          NA_character_
        } else {
          paste(unique(tied_plans), collapse = "; ")
        }
      },
      Sum.of.Amount.Debit = sum(Amount.Debit, na.rm = TRUE),
      Bill.Charge.Start.Date = safe_min_datetime(Bill.Charge.Start.Date),
      .source_order = first(.source_order),
      .groups = "drop"
    ) %>%
    select(
      Customer.Nbr,
      Contract.Number,
      Customer.Name,
      Unique.Id,
      Entity.Code,
      Entity.Name,
      Address,
      Mobile,
      Plan.Details,
      Sum.of.Amount.Debit,
      Bill.Charge.Start.Date,
      Bill.Charge.End.Date,
      .source_order
    )

  resolve_customer_overlaps(consolidated) %>%
    select(-.source_order) %>%
    arrange(Entity.Code, Customer.Nbr, Bill.Charge.Start.Date,
            Bill.Charge.End.Date, Contract.Number)
}

#function definitions....

area_wise_op <- function(wallet_in) {
  # get wallet input and filter on starting condition and export areawise
  WALLET_AREA_BPC = wallet_in %>% filter(str_detect(Entity.Code, "MSW"))
  write.csv(WALLET_AREA_BPC, "Output/Berhampore.csv", row.names = FALSE)
  WALLET_AREA_HLZ = wallet_in %>% filter(str_detect(Entity.Code, "HCS"))
  write.csv(WALLET_AREA_HLZ, "Output/Haldia.csv", row.names = FALSE)
  zip_area_files = paste(
    normalizePath(dirname(list.files(
      path = "Output/",
      pattern = "\\.csv$",
      ignore.case = TRUE,
      full.names = T
    ))),
    fsep = .Platform$file.sep,
    list.files(path = "Output/", pattern = "\\.csv$", ignore.case = TRUE),
    sep = ""
  )
  zip(
    zipfile = sprintf(
      "Output/Berhampore_Haldia_%s_%g",
      month(today() - months(1), label = TRUE, abbr = F),
      year(today())
    ),
    files = zip_area_files,
    flags = " a -tzip -sdel",
    zip = get_7zip_path()  # OS-independent path
  )
}

plan_wise_op <- function(wallet_in) {
  #filter plan wise and alacarte and other and output
  wallet_filtered_ala = filter(wallet_in, Plan.Details == "Alacarte Plan")
  write.csv(wallet_filtered_ala, "Alacarte.csv", row.names = FALSE)
  wallet_filtered_othr = filter(wallet_in, Plan.Details != "Alacarte Plan")
  write.csv(wallet_filtered_othr, "PlanBouqets.csv", row.names = FALSE)
}

lco_pivot_table <- function(wallet_in) {
  # The new wallet export is already invoice-only. Rows without a debit are
  # ignored for the amount summary, and direct entities are reported below.
  wallet_filt = wallet_in %>%
    filter(
      !(Entity.Code %in% direct_cus),
      !is.na(Amount.Debit)
    ) %>%
    select(Entity.Code, Amount.Debit)
  ### Following blocks summarises code wise debit amount and export file --pivot table
  lco_pivot = wallet_filt %>%
    group_by(Entity.Code) %>%
    summarize(Total_debit = sum(Amount.Debit, na.rm = TRUE), .groups = "drop")
  write.csv(
    lco_pivot,
    sprintf(
      "Output/LCOWISE_WALLET_SUMMARY_%s_%g.csv",
      month(today() - months(1), label = TRUE, abbr = F),
      year(rollback(today()))
    ),
    row.names = FALSE
  )
}

lcowise_data_export <- function(wallet_in) {
  ## filter lco code and remove duplicate then convert to a list for loop
  lco_list = wallet_in %>%
    filter(!(Entity.Code %in% direct_cus)) %>%
    select(Entity.Code) %>%
    distinct()
  lco_list = lco_list[['Entity.Code']]

  ## run the loop according to the list and export csv for each LCO
  for (lcocode in lco_list) {
    wallet_filtered = filter(wallet_in, Entity.Code == lcocode)
    write.csv(
      wallet_filtered,
      sprintf(
        "Output/%s_%s_%g.csv",
        lcocode,
        month(today() - months(1), label = TRUE, abbr = F),
        year(rollback(today()))
      ),
      row.names = FALSE
    )
    zip_lco_files = paste(
      normalizePath(dirname(list.files(
        path = "Output/",
        pattern = "\\.csv$",
        ignore.case = TRUE,
        full.names = T
      ))),
      fsep = .Platform$file.sep,
      list.files(path = "Output/", pattern = "\\.csv$", ignore.case = TRUE),
      sep = ""
    )
    zip(
      zipfile = sprintf(
        "Output/LCOWise_Wallet_Report_%s_%g",
        month(today() - months(1), label = TRUE, abbr = F),
        year(rollback(today()))
      ),
      files = zip_lco_files,
      flags = " a -tzip -sdel",
      zip = get_7zip_path()  # OS-independent path
    )
  }
}

#hdnd_nm = c('MDKH','MDBKT','MDBQA','MDCNDP','MDDHH','MDHCNJV','MDOR','MDSKWJV','TESTENTITY','CORP')

wallet = read.csv(
  file.choose(new = F),
  colClasses = c(
    Customer.Nbr = "character",
    Unique.Id = "character",
    Entity.Code = "character",
    Contract.Number = "character"
  ),
  stringsAsFactors = FALSE,
  check.names = TRUE,
  na.strings = c("", "NA")
)
#wallet = filter(wallet, !(Entity.Code %in% hdnd_nm))

area_wise_op(wallet)

#plan_wise_op(wallet)

# Direct entities are excluded inside both general LCO functions.
lcowise_data_export(wallet)

lco_pivot_table(wallet)


crdr = read.csv(file.choose(new = F), stringsAsFactors = FALSE) ##new credit note report from Reports screen
crdr1 = crdr %>% filter(Note.Type %in% c("CR", "DR"))
#crdr1 = crdr1 %>% filter(!(ENTITY_CODE %in% hdnd_nm))
write.csv(
  crdr1,
  sprintf(
    "Output/Credit_Debit_Note_%s_%g.csv",
    month(today() - months(1), label = TRUE, abbr = F),
    year(today())
  ),
  row.names = FALSE
)

####Find Direct customers bills####
direct_bill_detail = build_direct_bill_detail(wallet, direct_cus)

# Export one detail file per direct entity and package those files together.
direct_detail_files = character(0)
direct_detail_entities = sort(unique(direct_bill_detail$Entity.Code))
for (direct_entity in direct_detail_entities) {
  direct_detail_file = sprintf(
    "Output/Direct_customers_bill_detail_%s_%s_%g.csv",
    direct_entity,
    month(today() - months(1), label = TRUE, abbr = F),
    year(today())
  )
  write.csv(
    filter(direct_bill_detail, Entity.Code == direct_entity),
    direct_detail_file,
    row.names = FALSE
  )
  direct_detail_files = c(direct_detail_files, direct_detail_file)
}

if (length(direct_detail_files) > 0) {
  zip(
    zipfile = sprintf(
      "Output/Direct_customers_detail_bill_%s_%g",
      month(today() - months(1), label = TRUE, abbr = F),
      year(today())
    ),
    files = normalizePath(direct_detail_files, mustWork = TRUE),
    flags = " a -tzip",
    zip = get_7zip_path()
  )
}

#rtu cr note
rtu_cr = filter(crdr1, (Entity.Code %in% direct_cus)) %>%
  filter(Note.Type == "CR")
rtu_cr = rtu_cr %>%
  group_by(Entity.Code) %>%
  summarise(cr_note = sum(Adj.Value, na.rm = TRUE), .groups = "drop")
df = direct_bill_detail %>%
  group_by(Entity.Code, Entity.Name) %>%
  summarise(debit = sum(Sum.of.Amount.Debit, na.rm = TRUE), .groups = "drop")
#customer_dt = wallet %>% group_by(Customer.Nbr) %>% summarise(Tot_debit = sum(Amount.Debit))
final_bill = merge(
  df,
  rtu_cr,
  all.x = T,
  by.x = 'Entity.Code',
  by.y = 'Entity.Code'
)
final_bill = final_bill %>%
  mutate(
    cr_note = coalesce(cr_note, 0),
    Final_Bill = debit - cr_note
  )
write.csv(
  final_bill,
  sprintf(
    "Output/RTU_customers_bill_amount__%s_%g.csv",
    month(today() - months(1), label = TRUE, abbr = F),
    year(today())
  ),
  row.names = F
)
