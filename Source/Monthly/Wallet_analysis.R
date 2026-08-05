library(tidyverse)
library(dplyr)
library(readxl)
library(stringr)
library(lubridate)

# Helper function to get the correct 7-zip executable based on OS
get_7zip_path <- function() {
  os <- Sys.info()["sysname"]
  
  if (os == "Windows") {
    # Windows: Use standard 7-Zip installation path
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
    # Linux: Use system 7z command (install via: sudo apt install p7zip-full)
    path <- "7z"
    # Verify 7z is available
    if (system2("which", args = "7z", stdout = FALSE, stderr = FALSE) != 0) {
      stop("7-Zip not found. Please install it using: sudo apt install p7zip-full")
    }
    return(path)
    
  } else if (os == "Darwin") {
    # macOS: Use system 7z or Homebrew installed version
    path <- "7z"
    # Check common Homebrew path first
    if (file.exists("/usr/local/bin/7z")) {
      path <- "/usr/local/bin/7z"
    } else if (file.exists("/opt/homebrew/bin/7z")) {
      # Apple Silicon Mac
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
  wallet_filt = filter(wallet_in, Credit.Document.Type == "INVOICE") %>%
    select(Entity.Code, Amount.Debit)
  ### Following blocks summarises code wise debit amount and export file --pivot table
  lco_pivot = wallet_filt %>%
    group_by(Entity.Code) %>%
    summarize(Total_debit = sum(Amount.Debit))
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
  lco_list = wallet_in %>% select(Entity.Code) %>% distinct()
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

wallet = read.csv(file.choose(new = F), colClasses = c(Unique.Id = "character"))
#wallet = filter(wallet, !(Entity.Code %in% hdnd_nm))

area_wise_op(wallet)

#plan_wise_op(wallet)

lcowise_data_export(wallet)

lco_pivot_table(wallet)


crdr = read.csv(file.choose(new = F)) ##new credit note report from Reports screen
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

dq = c("INVOICE", "SELFCARE-CRNOTE")

####Find Direct customers bills####
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
wallet = filter(wallet, (Entity.Code %in% direct_cus))

wallet_filt = filter(wallet, Credit.Document.Type %in% dq) %>%
  select(
    Customer.Nbr,
    Customer.Name,
    Unique.Id,
    Entity.Code,
    Entity.Name,
    Plan.Details,
    Service.Name,
    Amount.Debit,
    Billing.Frequency,
    Transaction.Date
  )
wallet_filt$Amount.Debit = round(wallet_filt$Amount.Debit, digits = 2)

#rtu cr note
rtu_cr = filter(crdr1, (Entity.Code %in% direct_cus)) %>%
  filter(Note.Type == "CR")
rtu_cr = rtu_cr %>%
  group_by(Entity.Code) %>%
  summarise(cr_note = sum(Adj.Value))
df = wallet_filt %>%
  group_by(Entity.Code, Entity.Name) %>%
  summarise(debit = sum(Amount.Debit))
#customer_dt = wallet %>% group_by(Customer.Nbr) %>% summarise(Tot_debit = sum(Amount.Debit))
final_bill = merge(
  df,
  rtu_cr,
  all.x = T,
  by.x = 'Entity.Code',
  by.y = 'Entity.Code'
)
final_bill[is.na(final_bill)] <- 0
final_bill = final_bill %>% mutate(Final_Bill = debit - cr_note)
write.csv(
  final_bill,
  sprintf(
    "Output/RTU_customers_bill_amount__%s_%g.csv",
    month(today() - months(1), label = TRUE, abbr = F),
    year(today())
  ),
  row.names = F
)


