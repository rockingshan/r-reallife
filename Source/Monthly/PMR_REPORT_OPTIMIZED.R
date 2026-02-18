# ============================================================================
# PMR REPORT GENERATOR - OPTIMIZED VERSION
# ============================================================================
# Purpose: Generate Package Management Reports (PMR) for broadcaster settlements
# Author: Optimized from PMR_REPORT_MAKE_NEW.R
# Date: 2025-12-09
#
# Features:
# - Modular functions with clear responsibilities
# - Parameterized area processing (no duplication)
# - Automated file handling (optional interactive mode)
# - Consistent error handling
# - Progress tracking
# - Easy configuration
# ============================================================================

library(tidyverse)
library(dplyr)
library(readxl)
library(purrr)
library(officer)
library(flextable)
library(openxlsx)
library(lubridate)

# ============================================================================
# CONFIGURATION
# ============================================================================

CONFIG <- list(
  # Google Drive URLs
  urls = list(
    plan_names = "https://drive.google.com/u/0/uc?id=17GoiwT4nWCn0J_7HJF0ZyL5Y0-JPNwOJ&export=download",
    bouquet_names = "https://drive.google.com/u/0/uc?id=1INvieYzh6sc2O9XsMHVLI2d2LEOeuYT_&export=download",
    trai_names = "https://drive.google.com/u/0/uc?id=1I8UNHSbOoPBvQQ37QgVbMqBqEos10mtL&export=download"
  ),

  # Areas to process
  areas = list(
    list(name = "Network", filter_pattern = NULL, prefix = ""),
    list(name = "Berhampore", filter_pattern = "^MSW", prefix = "Berhampore_")
  ),

  # Output directory
  output_dir = "Output",

  # IPTV default state
  iptv_default_state = "West Bengal",

  # Plans to exclude
  excluded_plans = c('DD Channels', 'Platinum Digital Postpaid')
)

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Load data from Google Sheets
#' @param url Google Sheets export URL
#' @param encoding Character encoding (default: "UTF-8")
#' @return Dataframe
load_google_sheet <- function(url, encoding = "UTF-8") {
  tryCatch(
    {
      read.csv(url, encoding = encoding)
    },
    error = function(e) {
      message("Error loading Google Sheet: ", e$message)
      return(NULL)
    }
  )
}

#' Filter data by LCO area pattern
#' @param df Dataframe to filter
#' @param pattern Regex pattern for Lco.Code (NULL = no filter)
#' @return Filtered dataframe
filter_by_area <- function(df, pattern = NULL) {
  if (is.null(pattern)) {
    return(df)
  }
  df %>% filter(grepl(pattern, Lco.Code))
}

#' Save CSV with error handling
#' @param df Dataframe to save
#' @param filename Output filename
#' @param output_dir Output directory
safe_write_csv <- function(df, filename, output_dir = CONFIG$output_dir) {
  filepath <- file.path(output_dir, filename)
  tryCatch(
    {
      write.csv(df, filepath, row.names = FALSE)
      message("✓ Saved: ", filepath)
    },
    error = function(e) {
      message("✗ Error saving ", filename, ": ", e$message)
    }
  )
}

#' Add TRAI names and relocate column
#' @param df Dataframe
#' @param trai_names TRAI names lookup table
#' @param channel_col Name of channel column
#' @return Dataframe with TRAI names
add_trai_names <- function(df, trai_names, channel_col = "Channel") {
  df %>%
    merge(trai_names, by.x = channel_col, by.y = "Channel") %>%
    relocate(TRAI.name, .after = all_of(channel_col))
}

# ============================================================================
# DATA LOADING FUNCTIONS
# ============================================================================

#' Load all reference data from Google Sheets
#' @return List of reference dataframes
load_reference_data <- function() {
  message("Loading reference data from Google Sheets...")

  list(
    plan_names = load_google_sheet(CONFIG$urls$plan_names),
    bouquet_names = load_google_sheet(CONFIG$urls$bouquet_names),
    trai_names = load_google_sheet(CONFIG$urls$trai_names, encoding = "UTF-8")
  )
}

#' Load MQ data files interactively for CABLE processing
#' @return List of MQ dataframes
load_mq_data_interactive <- function() {
  message("\n=== LOAD CABLE MQ DATA FILES ===")
  message("Select MQ Bouquet data file...")
  list_bouquet_dated <- read.csv(file.choose(new = FALSE))

  message("Select MQ Alacarte data file...")
  list_alacarte <- read.csv(file.choose(new = FALSE))

  message("Select Package Details file...")
  pack_details <- read.csv(file.choose(new = FALSE))

  message("Select Plan Config file (with services)...")
  plan_config <- read.csv(file.choose(new = FALSE))

  list(
    list_bouquet_dated = list_bouquet_dated,
    list_alacarte = list_alacarte,
    pack_details = pack_details,
    plan_config = plan_config
  )
}

#' Process plan configuration
#' @param plan_config Raw plan config dataframe
#' @param pack_details Package details dataframe
#' @return Processed service-channel mapping
process_plan_config <- function(plan_config, pack_details) {
  # Filter for 30-day billing only
  plan_config_30d <- plan_config %>%
    filter(Billing.Frequency == "30D")

  # Merge with package details
  merged <- merge(plan_config_30d, pack_details)

  # Return unique service-channel mapping
  merged %>%
    select(Service.Name, Channel) %>%
    unique()
}

# ============================================================================
# CABLE PMR PROCESSING FUNCTIONS
# ============================================================================

#' Process Bronze Basic (FTA) channels
#' @param list_bouquet_dated Bouquet data
#' @param service_channel_map Service to channel mapping
#' @param trai_names TRAI names lookup
#' @return Dataframe with FTA channel counts
process_bronze_basic <- function(
  list_bouquet_dated,
  service_channel_map,
  trai_names
) {
  basic_bouquet <- list_bouquet_dated %>%
    filter(CHANNEL_NAME_5 == "Bronze Basic") %>%
    select(Cust.Id, CHANNEL_NAME_5, Plan.Name) %>%
    unique()

  Bronze_pivot <- basic_bouquet %>%
    group_by(Plan.Name, CHANNEL_NAME_5) %>%
    summarize(Active_count = n(), .groups = "drop")

  Bronze_merged <- merge(
    Bronze_pivot,
    service_channel_map,
    by.x = "CHANNEL_NAME_5",
    by.y = "Service.Name"
  ) %>%
    select(Plan.Name, CHANNEL_NAME_5, Channel, Active_count)

  colnames(Bronze_merged)[4] <- "Monthly.Subs.of.the.Channel"

  Bronze_final <- Bronze_merged %>%
    group_by(Channel) %>%
    summarize(Active_count = sum(Monthly.Subs.of.the.Channel), .groups = "drop")

  # Add TRAI names
  add_trai_names(Bronze_final, trai_names)
}

#' Process DPO bouquets
#' @param bc_bouquet_filtered Filtered bouquet data
#' @param plan_config Single pack configuration
#' @param service_channel_map Service to channel mapping
#' @return Dataframe with DPO bouquet data
process_dpo_bouquets <- function(
  bc_bouquet_filtered,
  plan_config,
  service_channel_map
) {
  bc_dpo_spread <- merge(bc_bouquet_filtered, plan_config)

  bc_dpo_bouq <- bc_dpo_spread %>%
    filter(X == "Bouquet")

  bc_dpo_bouq_merged <- merge(
    bc_dpo_bouq,
    service_channel_map,
    by.x = "Bouquet",
    by.y = "Service.Name"
  ) %>%
    select(Plan.Name, Bouquet, Channel, Active_count)

  colnames(bc_dpo_bouq_merged)[4] <- "Monthly.Subs.of.the.Channel"

  bc_dpo_bouq_merged %>%
    add_column(PackType = 'DPO Pack with broadcaster bouquets', .after = 2)
}

#' Process DPO alacarte
#' @param bc_dpo_spread DPO spread data
#' @return Dataframe with DPO alacarte data
process_dpo_alacarte <- function(bc_dpo_spread) {
  bc_bouquet_filtered_al <- bc_dpo_spread %>%
    filter(X == "Alacarte") %>%
    select(Plan.Name, Bouquet, Active_count) %>%
    add_column(PackType = 'DPO Pack with Alacarte', .after = 2) %>%
    mutate(Channel = Bouquet, .after = 3)

  colnames(bc_bouquet_filtered_al)[5] <- "Monthly.Subs.of.the.Channel"

  bc_bouquet_filtered_al
}

#' Process non-DPO broadcaster bouquets
#' @param Bouquet_merged Merged bouquet data
#' @param plan_names DPO plan names
#' @return Dataframe with broadcaster bouquet data
process_broadcaster_bouquets <- function(Bouquet_merged, plan_names) {
  bc_bouquet_filtered_noDPO <- Bouquet_merged %>%
    filter(!(Plan.Name %in% plan_names$Plan.Name)) %>%
    filter(!(Plan.Name %in% CONFIG$excluded_plans)) %>%
    add_column(PackType = 'Broadcaster Bouquets', .after = 2)

  colnames(bc_bouquet_filtered_noDPO)[2] <- "Bouquet"

  bc_bouquet_filtered_noDPO
}

#' Generate complete cable PMR reports for an area
#' @param list_bouquet_dated Bouquet data
#' @param list_alacarte Alacarte data
#' @param service_channel_map Service-channel mapping
#' @param plan_names DPO plan names
#' @param trai_names TRAI names
#' @param plan_config Single pack config
#' @param area_config Area configuration (name, filter_pattern, prefix)
#' @return List of report dataframes
generate_cable_pmr <- function(
  list_bouquet_dated,
  list_alacarte,
  service_channel_map,
  plan_names,
  trai_names,
  plan_config,
  area_config
) {
  message(sprintf("\n=== Processing Cable PMR for %s ===", area_config$name))

  # Filter by area if needed
  list_bouquet_dated <- filter_by_area(
    list_bouquet_dated,
    area_config$filter_pattern
  )
  list_alacarte <- filter_by_area(list_alacarte, area_config$filter_pattern)

  # 1. Process Bronze Basic (FTA)
  message("  Processing Bronze Basic (FTA) channels...")
  fta_report <- process_bronze_basic(
    list_bouquet_dated,
    service_channel_map,
    trai_names
  )
  safe_write_csv(fta_report, paste0(area_config$prefix, "FTA_Channels.csv"))

  # 2. Process all bouquets (excluding Bronze Basic)
  message("  Processing bouquets...")
  bq_try <- list_bouquet_dated %>%
    group_by(Plan.Name, CHANNEL_NAME_5) %>%
    select(Cust.Id, CHANNEL_NAME_5, Plan.Name) %>%
    unique() %>%
    summarize(Active_count = n(), .groups = "drop") %>%
    filter(CHANNEL_NAME_5 != "Bronze Basic")

  Bouquet_merged <- merge(
    bq_try,
    service_channel_map,
    by.x = "CHANNEL_NAME_5",
    by.y = "Service.Name"
  ) %>%
    select(Plan.Name, CHANNEL_NAME_5, Channel, Active_count)

  colnames(Bouquet_merged)[4] <- "Monthly.Subs.of.the.Channel"

  # 3. Filter for DPO plans
  bc_bouquet_filtered <- bq_try %>%
    filter(Plan.Name %in% plan_names$Plan.Name) %>%
    select(Plan.Name, Active_count)

  # 4. Merge and categorize
  bc_dpo_spread <- merge(bc_bouquet_filtered, plan_config)

  bc_dpo_bouq_merged <- process_dpo_bouquets(
    bc_bouquet_filtered,
    plan_config,
    service_channel_map
  )
  bc_bouquet_filtered_al <- process_dpo_alacarte(bc_dpo_spread)
  bc_bouquet_filtered_noDPO <- process_broadcaster_bouquets(
    Bouquet_merged,
    plan_names
  )

  # 5. Combine all bouquet types
  message("  Combining all bouquet types...")
  bC_bouqet_final <- rbind(
    bc_dpo_bouq_merged,
    bc_bouquet_filtered_al,
    bc_bouquet_filtered_noDPO
  )
  bC_bouqet_final$Monthly.Subs.of.the.Channel <- as.numeric(
    bC_bouqet_final$Monthly.Subs.of.the.Channel
  )

  # 6. Create pivot by channel and package type
  bC_bouqet_final_pivot <- bC_bouqet_final %>%
    group_by(Channel, PackType) %>%
    summarize(
      TotalMonthlySubs = sum(Monthly.Subs.of.the.Channel),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = PackType, values_from = TotalMonthlySubs)

  # 7. Add TRAI names and reorganize columns
  bc_bq_final_pvt_trai <- add_trai_names(bC_bouqet_final_pivot, trai_names) %>%
    relocate("DPO Pack with Alacarte", .after = "Broadcaster Bouquets")

  bc_bq_final_pvt_trai[is.na(bc_bq_final_pvt_trai)] <- 0

  # 8. Process alacarte
  message("  Processing alacarte channels...")
  ala_count <- list_alacarte %>%
    group_by(Channel.Name) %>%
    summarise(Monthly.Subs.of.the.Channel = n(), .groups = "drop")

  colnames(ala_count)[1] <- "Channel"
  bc_ala_pvt_trai <- add_trai_names(ala_count, trai_names)

  # 9. Save reports
  message("  Saving bouquet and alacarte reports...")
  safe_write_csv(
    bc_bq_final_pvt_trai,
    paste0(area_config$prefix, "Broadcaster Bouquet report PMR.csv")
  )
  safe_write_csv(
    bc_ala_pvt_trai,
    paste0(area_config$prefix, "Broadcaster Alacarte report PMR.csv")
  )

  # 10. Quarterly reports
  message("  Generating quarterly reports...")
  all_bouquet <- bq_try %>%
    filter(!(Plan.Name %in% plan_names$Plan.Name)) %>%
    group_by(Plan.Name) %>%
    summarize(Total = sum(Active_count), .groups = "drop")

  safe_write_csv(all_bouquet, paste0(area_config$prefix, "Bouqet_count.csv"))

  all_dpopack <- bq_try %>%
    filter(Plan.Name %in% plan_names$Plan.Name) %>%
    group_by(Plan.Name) %>%
    summarize(Total = sum(Active_count), .groups = "drop")

  safe_write_csv(all_dpopack, paste0(area_config$prefix, "DPO_count.csv"))

  # Return subscriber counts
  bq_subs <- list_bouquet_dated %>% select(Cust.Id, Lco.Code) %>% unique()
  ala_subs <- list_alacarte %>% select(Cust.Id, Lco.Code) %>% unique()
  all_sub <- rbind(bq_subs, ala_subs) %>% unique()

  message(sprintf(
    "  Total unique subscribers for %s: %d",
    area_config$name,
    nrow(all_sub)
  ))

  return(list(
    fta = fta_report,
    bouquet = bc_bq_final_pvt_trai,
    alacarte = bc_ala_pvt_trai,
    subscribers = all_sub
  ))
}

# ============================================================================
# IPTV PMR PROCESSING FUNCTIONS
# ============================================================================

#' Load IPTV data files
#' @return List of IPTV dataframes
load_iptv_data_interactive <- function() {
  message("\n=== LOAD IPTV DATA FILES ===")
  message("Select Main IPTV subscription file (Excel)...")
  main_file <- file.choose()

  message("\n=== LOAD IPTV SINGLE PACK CONFIGS (4 weeks) ===")
  message(
    "NOTE: These are IPTV-specific single pack configs (different from Cable)"
  )

  message("\nSelect IPTV Single Pack Config - 7th day...")
  singlepack_7 <- read.csv(file.choose())

  message("Select IPTV Single Pack Config - 14th day...")
  singlepack_14 <- read.csv(file.choose())

  message("Select IPTV Single Pack Config - 21st day...")
  singlepack_21 <- read.csv(file.choose())

  message("Select IPTV Single Pack Config - 28th day...")
  singlepack_28 <- read.csv(file.choose())

  list(
    main_file = main_file,
    singlepack_7 = singlepack_7,
    singlepack_14 = singlepack_14,
    singlepack_21 = singlepack_21,
    singlepack_28 = singlepack_28
  )
}

#' Clean and prepare IPTV data
#' @param main_file Path to main IPTV Excel file
#' @param default_state Default state for blank entries
#' @return Cleaned IPTV dataframe
clean_iptv_data <- function(
  main_file,
  default_state = CONFIG$iptv_default_state
) {
  # Load main data
  main_data <- read_excel(main_file, sheet = 1, skip = 2)

  # Rename columns
  colnames(main_data) <- c(
    "Plan_Name",
    "Date",
    "Account_No",
    "Subscriber",
    "Mobile_No",
    "STB_No",
    "VC_Number",
    "User_ID",
    "From_Date",
    "To_Date",
    "Partner_Name",
    "State",
    "Plan_Code"
  )

  # Clean and transform
  main_data %>%
    mutate(
      Date = as.Date(Date),
      From_Date = as.Date(From_Date),
      To_Date = as.Date(To_Date),
      Plan_Type = ifelse(
        grepl("HD", Plan_Name, ignore.case = TRUE),
        "HD",
        "SD"
      ),
      State = case_when(
        is.na(State) | State == "" | trimws(State) == "" ~ default_state,
        TRUE ~ trimws(State)
      )
    ) %>%
    filter(!is.na(Date), !is.na(User_ID))
}

#' Extract 4-week snapshot data from IPTV
#' @param main_data Cleaned IPTV data
#' @return Summary table with 4-week snapshots
extract_iptv_snapshots <- function(main_data) {
  # Get target dates (7th, 14th, 21st, 28th)
  target_dates <- main_data %>%
    filter(format(Date, "%d") %in% c("07", "14", "21", "28")) %>%
    pull(Date) %>%
    unique() %>%
    sort()

  # Generate dynamic labels
  day_parts <- format(target_dates, "%d")
  day_labels <- paste0("No.of.Subs.On.", as.integer(day_parts), "th.Day")
  names(day_labels) <- as.character(target_dates)

  # Create summary table
  summary_table <- main_data %>%
    group_by(Plan_Name, Plan_Code, Date) %>%
    summarise(Unique_Subs = n_distinct(User_ID), .groups = "drop") %>%
    mutate(Date_Label = day_labels[as.character(Date)]) %>%
    filter(!is.na(Date_Label)) %>%
    select(-Date) %>%
    pivot_wider(
      names_from = Date_Label,
      values_from = Unique_Subs,
      values_fill = 0
    )

  # Aggregate by Plan_Code
  summary_table %>%
    group_by(Plan_Code) %>%
    summarise(
      across(contains("Subs"), \(x) sum(x, na.rm = TRUE)),
      .groups = "drop"
    )
}

#' Merge IPTV data with single pack configs
#' @param iptvreport IPTV report with 4-week data
#' @param singlepack_list List of 4 single pack configs
#' @return Combined IPTV data with package info
merge_iptv_packages <- function(iptvreport, singlepack_list) {
  # Extract data for each day
  iptv_nw_7 <- iptvreport %>%
    select(Plan_Code, any_of("No.of.Subs.On.7th.Day")) %>%
    rename_with(~"No.of.Subs.On.7th.Day", contains("7th"))

  iptv_nw_14 <- iptvreport %>%
    select(Plan_Code, any_of("No.of.Subs.On.14th.Day")) %>%
    rename_with(~"No.of.Subs.On.14th.Day", contains("14th"))

  iptv_nw_21 <- iptvreport %>%
    select(Plan_Code, any_of("No.of.Subs.On.21th.Day")) %>%
    rename_with(~"No.of.Subs.On.21th.Day", contains("21"))

  iptv_nw_28 <- iptvreport %>%
    select(Plan_Code, any_of("No.of.Subs.On.28th.Day")) %>%
    rename_with(~"No.of.Subs.On.28th.Day", contains("28th"))

  # Merge with package data
  iptv_nw_7_pk <- merge(
    iptv_nw_7,
    singlepack_list$singlepack_7,
    by.x = "Plan_Code",
    by.y = "Code",
    all.y = TRUE
  ) %>%
    unique() %>%
    unite(combined, c('Plan_Code', 'Bouquet'), sep = "|")

  iptv_nw_14_pk <- merge(
    iptv_nw_14,
    singlepack_list$singlepack_14,
    by.x = "Plan_Code",
    by.y = "Code",
    all.y = FALSE
  ) %>%
    unique() %>%
    unite(combined, c('Plan_Code', 'Bouquet'), sep = "|")

  iptv_nw_21_pk <- merge(
    iptv_nw_21,
    singlepack_list$singlepack_21,
    by.x = "Plan_Code",
    by.y = "Code",
    all.y = FALSE
  ) %>%
    unique() %>%
    unite(combined, c('Plan_Code', 'Bouquet'), sep = "|")

  iptv_nw_28_pk <- merge(
    iptv_nw_28,
    singlepack_list$singlepack_28,
    by.x = "Plan_Code",
    by.y = "Code",
    all.y = FALSE
  ) %>%
    unique() %>%
    unite(combined, c('Plan_Code', 'Bouquet'), sep = "|")

  # Combine all weeks
  iptv_combo <- merge(
    iptv_nw_7_pk,
    iptv_nw_14_pk,
    by = "combined",
    all = TRUE
  ) %>%
    merge(iptv_nw_21_pk, by = "combined", all = TRUE) %>%
    merge(iptv_nw_28_pk, all = TRUE) %>%
    separate(combined, into = c("Code", "Bouquet"), sep = "\\|")

  # Convert to numeric and calculate average
  iptv_combo[is.na(iptv_combo)] <- 0
  iptv_combo$No.of.Subs.On.7th.Day <- as.numeric(
    iptv_combo$No.of.Subs.On.7th.Day
  )
  iptv_combo$No.of.Subs.On.14th.Day <- as.numeric(
    iptv_combo$No.of.Subs.On.14th.Day
  )
  iptv_combo$No.of.Subs.On.21th.Day <- as.numeric(
    iptv_combo$No.of.Subs.On.21th.Day
  )
  iptv_combo$No.of.Subs.On.28th.Day <- as.numeric(
    iptv_combo$No.of.Subs.On.28th.Day
  )

  iptv_combo %>%
    mutate(
      Monthly.Subs.of.the.Channel = rowMeans(select(., starts_with("No.of")))
    )
}

#' Process IPTV bouquet data
#' @param iptv_combo Combined IPTV data
#' @param service_channel_map Service-channel mapping
#' @param trai_names TRAI names
#' @return Processed bouquet report
process_iptv_bouquet <- function(iptv_combo, service_channel_map, trai_names) {
  # Filter bouquets
  iptv_combo_bouq <- iptv_combo %>%
    filter(X == 'Bouquet') %>%
    select(
      Broadcaster.Name,
      Bouquet,
      No.of.Subs.On.7th.Day,
      No.of.Subs.On.14th.Day,
      No.of.Subs.On.21th.Day,
      No.of.Subs.On.28th.Day,
      Monthly.Subs.of.the.Channel
    )

  # Get broadcaster names
  bc_name <- iptv_combo_bouq %>%
    select(Broadcaster.Name, Bouquet) %>%
    distinct() %>%
    na.omit()

  # Ensure numeric
  iptv_combo_bouq$No.of.Subs.On.7th.Day <- as.numeric(
    iptv_combo_bouq$No.of.Subs.On.7th.Day
  )
  iptv_combo_bouq$No.of.Subs.On.14th.Day <- as.numeric(
    iptv_combo_bouq$No.of.Subs.On.14th.Day
  )
  iptv_combo_bouq$No.of.Subs.On.21th.Day <- as.numeric(
    iptv_combo_bouq$No.of.Subs.On.21th.Day
  )
  iptv_combo_bouq$No.of.Subs.On.28th.Day <- as.numeric(
    iptv_combo_bouq$No.of.Subs.On.28th.Day
  )
  iptv_combo_bouq$Monthly.Subs.of.the.Channel <- as.numeric(
    iptv_combo_bouq$Monthly.Subs.of.the.Channel
  )

  # Create pivot
  active_pivot <- iptv_combo_bouq %>%
    group_by(Bouquet) %>%
    summarize(
      'Active_7th' = sum(No.of.Subs.On.7th.Day),
      'Active_14th' = sum(No.of.Subs.On.14th.Day),
      'Active_21th' = sum(No.of.Subs.On.21th.Day),
      'Active_28th' = sum(No.of.Subs.On.28th.Day),
      'Average' = sum(Monthly.Subs.of.the.Channel),
      .groups = "drop"
    )

  # Merge with broadcaster names
  od_bq_rpt <- merge(bc_name, active_pivot)

  # Map to channels
  Bouquet_merged <- merge(
    od_bq_rpt,
    service_channel_map,
    by.x = "Bouquet",
    by.y = "Service.Name"
  ) %>%
    group_by(Channel) %>%
    summarise(Total = sum(Average), .groups = "drop")

  # Add TRAI names
  add_trai_names(Bouquet_merged, trai_names)
}

#' Process IPTV alacarte data
#' @param iptv_combo Combined IPTV data
#' @param trai_names TRAI names
#' @return Processed alacarte report
process_iptv_alacarte <- function(iptv_combo, trai_names) {
  # Filter alacarte
  iptv_combo_ala <- iptv_combo %>%
    filter(X == 'Alacarte') %>%
    select(
      Broadcaster.Name,
      Bouquet,
      No.of.Subs.On.7th.Day,
      No.of.Subs.On.14th.Day,
      No.of.Subs.On.21th.Day,
      No.of.Subs.On.28th.Day,
      Monthly.Subs.of.the.Channel
    )

  colnames(iptv_combo_ala)[2] <- 'Channel'

  # Get broadcaster names
  bc_name <- iptv_combo_ala %>%
    select(Broadcaster.Name, Channel) %>%
    distinct() %>%
    na.omit()

  # Create pivot
  active_pivot <- iptv_combo_ala %>%
    group_by(Channel) %>%
    summarize(
      'Active_7th' = sum(No.of.Subs.On.7th.Day),
      'Active_14th' = sum(No.of.Subs.On.14th.Day),
      'Active_21th' = sum(No.of.Subs.On.21th.Day),
      'Active_28th' = sum(No.of.Subs.On.28th.Day),
      'Average' = sum(Monthly.Subs.of.the.Channel),
      .groups = "drop"
    )

  # Merge and add TRAI names
  od_al_rpt <- merge(bc_name, active_pivot)

  od_al_rpt %>%
    merge(trai_names, by.x = "Channel", by.y = "Channel") %>%
    relocate(TRAI.name, .after = Channel) %>%
    select(Channel, TRAI.name, Average)
}

#' Generate IPTV PMR reports
#' @param iptv_data IPTV data list
#' @param service_channel_map Service-channel mapping
#' @param trai_names TRAI names
#' @return List of IPTV reports
generate_iptv_pmr <- function(iptv_data, service_channel_map, trai_names) {
  message("\n=== Processing IPTV PMR ===")

  # Clean data
  message("  Cleaning IPTV data...")
  main_data <- clean_iptv_data(iptv_data$main_file)

  # Extract snapshots
  message("  Extracting 4-week snapshots...")
  iptvreport <- extract_iptv_snapshots(main_data)

  # Merge with packages
  message("  Merging with package configurations...")
  iptv_combo <- merge_iptv_packages(iptvreport, iptv_data)

  # Process bouquets
  message("  Processing IPTV bouquets...")
  bouquet_report <- process_iptv_bouquet(
    iptv_combo,
    service_channel_map,
    trai_names
  )
  safe_write_csv(bouquet_report, "IPTV_DPO_bouquet_count.csv")

  # Process alacarte
  message("  Processing IPTV alacarte...")
  alacarte_report <- process_iptv_alacarte(iptv_combo, trai_names)
  safe_write_csv(alacarte_report, "IPTV_DPO_Alacarte.csv")

  message("  IPTV PMR completed!")

  return(list(
    bouquet = bouquet_report,
    alacarte = alacarte_report
  ))
}

# ============================================================================
# MAIN EXECUTION FUNCTION
# ============================================================================

#' Main function to generate all PMR reports
#' @param process_cable Boolean - process cable PMR (default: TRUE)
#' @param process_iptv Boolean - process IPTV PMR (default: TRUE)
#' @param interactive Boolean - use interactive file selection (default: TRUE)
main_generate_pmr <- function(
  process_cable = TRUE,
  process_iptv = TRUE,
  interactive = TRUE
) {
  message("╔════════════════════════════════════════════════════════════╗")
  message("║       PMR REPORT GENERATOR - OPTIMIZED VERSION             ║")
  message("╚════════════════════════════════════════════════════════════╝")

  results <- list()

  # Load reference data
  ref_data <- load_reference_data()

  # Initialize service_channel_map (will be set based on what's being processed)
  service_channel_map <- NULL

  if (process_cable) {
    # Load MQ data
    mq_data <- load_mq_data_interactive()

    # Process plan config for CABLE
    service_channel_map <- process_plan_config(
      mq_data$plan_config,
      mq_data$pack_details
    )

    # Load single pack config for Cable DPO processing
    message(
      "\n=== CABLE TV: Select Single Pack Config file for DPO processing ==="
    )
    cable_plan_config <- read.csv(file.choose())

    # Process each area
    results$cable <- list()
    for (area_config in CONFIG$areas) {
      area_result <- generate_cable_pmr(
        list_bouquet_dated = mq_data$list_bouquet_dated,
        list_alacarte = mq_data$list_alacarte,
        service_channel_map = service_channel_map,
        plan_names = ref_data$plan_names,
        trai_names = ref_data$trai_names,
        plan_config = cable_plan_config,
        area_config = area_config
      )
      results$cable[[area_config$name]] <- area_result
    }
  }

  if (process_iptv) {
    # If Cable was not processed, we need to create service_channel_map for IPTV
    if (is.null(service_channel_map)) {
      message("\n=== Loading Package Details for IPTV ===")
      message("Select Package Details file...")
      pack_details <- read.csv(file.choose())
      message("Select Plan Config file (30-day billing)...")
      plan_config_iptv <- read.csv(file.choose())

      # Create service_channel_map for IPTV
      service_channel_map <- process_plan_config(plan_config_iptv, pack_details)
    }

    # Load IPTV data (includes IPTV-specific single pack configs)
    iptv_data <- load_iptv_data_interactive()

    # Process IPTV PMR
    results$iptv <- generate_iptv_pmr(
      iptv_data = iptv_data,
      service_channel_map = service_channel_map,
      trai_names = ref_data$trai_names
    )
  }

  message("\n╔════════════════════════════════════════════════════════════╗")
  message("║              PMR GENERATION COMPLETED!                      ║")
  message("╚════════════════════════════════════════════════════════════╝")

  return(results)
}

# ============================================================================
# EXECUTION
# ============================================================================

# Run the main function
# You can customize which reports to generate:
#   main_generate_pmr(process_cable = TRUE, process_iptv = TRUE)   # Both
#   main_generate_pmr(process_cable = TRUE, process_iptv = FALSE)  # Cable only
#   main_generate_pmr(process_cable = FALSE, process_iptv = TRUE)  # IPTV only

results <- main_generate_pmr(process_cable = TRUE, process_iptv = TRUE)

# ============================================================================
# END OF SCRIPT
# ============================================================================
