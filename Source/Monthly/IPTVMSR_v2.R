# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(officer)
library(flextable)
library(openxlsx)
library(lubridate)

# --- LOAD MAIN DATA FILE ---
main_file <- file.choose() # Main IPTV subscription file
singlepack_7 <- read.csv(file.choose())
singlepack_14 <- read.csv(file.choose())
singlepack_21 <- read.csv(file.choose())
singlepack_28 <- read.csv(file.choose())

# Load main data
main_data <- read_excel(main_file, sheet = 1, skip = 2)

# --- CLEAN AND PREPARE DATA ---
# Rename columns based on your sample structure
colnames(main_data) <- c(
  "Plan_Name", "Date", "Account_No", "Subscriber", "Mobile_No",
  "STB_No", "VC_Number", "User_ID", "From_Date", "To_Date", 
  "Partner_Name", "State", "Plan_Code"
)

# Convert date columns and clean data
main_data <- main_data %>%
  mutate(
    Date = as.Date(Date),
    From_Date = as.Date(From_Date),
    To_Date = as.Date(To_Date),
    Plan_Type = ifelse(grepl("HD", Plan_Name, ignore.case = TRUE), "HD", "SD"),
    # Clean State column: replace blank/NA with "West Bengal"
    State = case_when(
      is.na(State) | State == "" | trimws(State) == "" ~ "West Bengal",
      TRUE ~ trimws(State)
    )
  ) %>%
  filter(!is.na(Date), !is.na(User_ID)) # Remove rows with missing critical data

# --- APPLY STATE CONSOLIDATION RULES ---
apply_state_consolidation <- function(df) {
  # Count subscribers by state and date
  state_date_counts <- df %>%
    group_by(State, Date) %>%
    summarise(subscriber_count = n_distinct(User_ID), .groups = "drop")
  
  # Find states that have less than 10 subscribers on ANY date
  states_to_consolidate <- state_date_counts %>%
    filter(subscriber_count < 10) %>%
    pull(State) %>%
    unique()
  
  # Update states with low subscriber counts to "West Bengal"
  df_consolidated <- df %>%
    mutate(
      Original_State = State,
      State = case_when(
        State %in% states_to_consolidate ~ "West Bengal",
        TRUE ~ State
      )
    )
  
  cat("States consolidated into West Bengal due to <10 subscribers:", 
      paste(states_to_consolidate, collapse = ", "), "\n")
  
  return(df_consolidated)
}

# Apply consolidation
main_data_consolidated <- apply_state_consolidation(main_data)

# Get list of final states for processing
final_states <- unique(main_data_consolidated$State)
cat("Final states for processing:", paste(final_states, collapse = ", "), "\n")

# --- UPDATED STATE SUMMARY FUNCTION ---
state_summary <- function(df, state_name) {
  # Filter data for specific state
  state_data <- df %>% filter(State == state_name)
  
  if(nrow(state_data) == 0) {
    return(NULL)
  }
  
  # Count unique User_IDs per date and plan type
  counts <- state_data %>%
    group_by(Date, Plan_Type) %>%
    summarise(User_Count = n_distinct(User_ID), .groups = "drop") %>%
    pivot_wider(
      names_from = Plan_Type,
      values_from = User_Count,
      values_fill = list(User_Count = 0)
    ) %>%
    mutate(
      HD = if (!"HD" %in% names(.)) 0 else HD,
      SD = if (!"SD" %in% names(.)) 0 else SD,
      `HD+SD` = HD
    )
  
  # Create summary table
  summary_table <- tibble(
    Type = c(
      "Total Unique Active Paying Subscribers (HD+SD)",
      "Total Unique Active Paying Subscribers (SD)"
    )
  )
  
  # Add dynamic date columns
  for (d in sort(unique(counts$Date))) {
    date_col <- format(as.Date(d), "%Y-%m-%d")
    hd_sd_total <- counts %>% filter(Date == d) %>% pull(`HD+SD`)
    sd_only <- counts %>% filter(Date == d) %>% pull(SD)
    
    if(length(hd_sd_total) == 0) hd_sd_total <- 0
    if(length(sd_only) == 0) sd_only <- 0
    
    summary_table[[date_col]] <- c(hd_sd_total, sd_only)
  }
  
  # Add Average
  summary_table <- summary_table %>%
    rowwise() %>%
    mutate(Average = mean(c_across(-Type), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(STATE = state_name) %>%
    select(STATE, everything())
  
  return(summary_table)
}

subs_summary <- function(df_raw, state_name, singlepack_7, singlepack_14, singlepack_21, singlepack_28) {
  # Filter data for specific state
  state_data <- df_raw %>% filter(State == state_name)
  
  if(nrow(state_data) == 0) {
    cat("No data found for state:", state_name, "\n")
    return(NULL)
  }
  
  # Get 7th, 14th, 21st, 28th from the Date column
  target_dates <- state_data %>%
    filter(format(Date, "%d") %in% c("07", "14", "21", "28")) %>%
    pull(Date) %>%
    unique() %>%
    sort()
  
  if(length(target_dates) == 0) {
    cat("No target dates (7th, 14th, 21st, 28th) found for state:", state_name, "\n")
    return(NULL)
  }
  
  # Generate dynamic labels
  day_parts <- format(target_dates, "%d")
  day_labels <- paste0("No.of.Subs.On.", as.integer(day_parts), "th.Day")
  names(day_labels) <- as.character(target_dates)
  
  # Create summary table using Plan_Code directly (no merge needed)
  summary_table <- state_data %>%
    group_by(Plan_Name, Plan_Code, Date) %>%
    summarise(Unique_Subs = n_distinct(User_ID), .groups = "drop") %>%
    mutate(Date_Label = day_labels[as.character(Date)]) %>%
    filter(!is.na(Date_Label)) %>%  # Only keep target dates
    select(-Date) %>%
    pivot_wider(names_from = Date_Label, values_from = Unique_Subs, values_fill = 0)
  
  if(nrow(summary_table) == 0) {
    cat("No summary data created for state:", state_name, "\n")
    return(NULL)
  }
  
  # Aggregate by Plan_Code
  iptvreport <- summary_table %>% 
    group_by(Plan_Code) %>%
    summarise(across(contains("Subs"), \(x) sum(x, na.rm = TRUE)), .groups = "drop")
  
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
  iptv_nw_7_pk <- merge(iptv_nw_7, singlepack_7, by.x = "Plan_Code", by.y = "Code", all.y = TRUE) %>% 
    unique() %>% 
    unite(combined, c('Plan_Code','Bouquet'), sep = "|")
  
  iptv_nw_14_pk <- merge(iptv_nw_14, singlepack_14, by.x = "Plan_Code", by.y = "Code", all.y = FALSE) %>% 
    unique() %>% 
    unite(combined, c('Plan_Code','Bouquet'), sep = "|")
  
  iptv_nw_21_pk <- merge(iptv_nw_21, singlepack_21, by.x = "Plan_Code", by.y = "Code", all.y = FALSE) %>% 
    unique() %>% 
    unite(combined, c('Plan_Code','Bouquet'), sep = "|")
  
  iptv_nw_28_pk <- merge(iptv_nw_28, singlepack_28, by.x = "Plan_Code", by.y = "Code", all.y = FALSE) %>% 
    unique() %>% 
    unite(combined, c('Plan_Code','Bouquet'), sep = "|")
  
  # Combine all data
  iptv_combo = merge(iptv_nw_7_pk,iptv_nw_14_pk, by.x = "combined", by.y = "combined", all = T)
  iptv_combo = merge(iptv_combo,iptv_nw_21_pk, by.x = "combined", by.y = "combined", all = T)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
  iptv_combo = merge(iptv_combo, iptv_nw_28_pk, all = T) %>% separate(combined, into = c("Code","Bouquet"),sep = "\\|")
  iptv_combo[is.na(iptv_combo)] <- 0
  iptv_combo$No.of.Subs.On.7th.Day = as.numeric(iptv_combo$No.of.Subs.On.7th.Day)
  iptv_combo$No.of.Subs.On.14th.Day = as.numeric(iptv_combo$No.of.Subs.On.14th.Day)
  iptv_combo$No.of.Subs.On.21th.Day = as.numeric(iptv_combo$No.of.Subs.On.21th.Day)
  iptv_combo$No.of.Subs.On.28th.Day = as.numeric(iptv_combo$No.of.Subs.On.28th.Day)
  iptv_combo = iptv_combo %>% mutate(Monthly.Subs.of.the.Channel = rowMeans(select(iptv_combo, starts_with("No.of"))))
  
  # Process Bouquet data
  iptv_combo_bouq = iptv_combo %>% filter(X == 'Bouquet') %>% select(Broadcaster.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day,
                                                                     No.of.Subs.On.21th.Day,No.of.Subs.On.28th.Day,Monthly.Subs.of.the.Channel)
  
  bc_name = iptv_combo_bouq %>%
    select(Broadcaster.Name,Bouquet) %>% distinct()%>% na.omit()
  
  iptv_combo_bouq$No.of.Subs.On.7th.Day = as.numeric(iptv_combo_bouq$No.of.Subs.On.7th.Day)
  iptv_combo_bouq$No.of.Subs.On.14th.Day = as.numeric(iptv_combo_bouq$No.of.Subs.On.14th.Day)
  iptv_combo_bouq$No.of.Subs.On.21th.Day = as.numeric(iptv_combo_bouq$No.of.Subs.On.21th.Day)
  iptv_combo_bouq$No.of.Subs.On.28th.Day = as.numeric(iptv_combo_bouq$No.of.Subs.On.28th.Day)
  iptv_combo_bouq$Monthly.Subs.of.the.Channel = as.numeric(iptv_combo_bouq$Monthly.Subs.of.the.Channel)
  
  active_pivot = iptv_combo_bouq %>% 
    group_by(Bouquet) %>%
    summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day),'Active_21th' = sum(No.of.Subs.On.21th.Day),
              'Active_28th' = sum(No.of.Subs.On.28th.Day),'Average' = sum(Monthly.Subs.of.the.Channel))
  od_bq_rpt = merge(bc_name,active_pivot)
  
  iptv_combo_ala = iptv_combo %>% filter(X == 'Alacarte') %>% select(Broadcaster.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day,
                                                                     No.of.Subs.On.21th.Day,No.of.Subs.On.28th.Day,Monthly.Subs.of.the.Channel)
  colnames(iptv_combo_ala)[2]<-'Channel'
  
  bc_name = iptv_combo_ala %>%
    select(Broadcaster.Name,Channel) %>% distinct()%>% na.omit()
  
  active_pivot = iptv_combo_ala %>% 
    group_by(Channel) %>%
    summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day),'Active_21th' = sum(No.of.Subs.On.21th.Day),
              'Active_28th' = sum(No.of.Subs.On.28th.Day),'Average' = sum(Monthly.Subs.of.the.Channel))
  od_al_rpt = merge(bc_name,active_pivot)
  
  
  # Create output directory if it doesn't exist
  if(!dir.exists("Output")) {
    dir.create("Output")
  }
  
  # Export to Excel with dynamic naming using openxlsx
  file_name <- sprintf("Output/%s_IPTV_MSR_all_%s_%g.xlsx", 
                       gsub(" ", "_", state_name),
                       month(today() - months(1), label = TRUE, abbr = FALSE),
                       year(today()))
  
  # Create workbook
  wb <- createWorkbook()
  
  # Add Bouquet sheet if data exists
  if(nrow(od_bq_rpt) > 0) {
    addWorksheet(wb, "Bouquet")
    writeData(wb, "Bouquet", od_bq_rpt, rowNames = FALSE)
    cat("Added Bouquet sheet with", nrow(od_bq_rpt), "rows\n")
  }
  
  # Add Alacarte sheet if data exists
  if(nrow(od_al_rpt) > 0) {
    addWorksheet(wb, "Alacarte")
    writeData(wb, "Alacarte", od_al_rpt, rowNames = FALSE)
    cat("Added Alacarte sheet with", nrow(od_al_rpt), "rows\n")
  }
  
  # Save workbook only if we have at least one sheet
  if(length(names(wb)) > 0) {
    saveWorkbook(wb, file_name, overwrite = TRUE)
    cat("Excel file saved successfully:", file_name, "\n")
    
    # Verify file exists
    if(file.exists(file_name)) {
      file_size <- file.info(file_name)$size
      cat("File confirmed - Size:", file_size, "bytes\n")
    } else {
      cat("WARNING: File was not created successfully!\n")
    }
  } else {
    cat("No data to write for state:", state_name, "\n")
  }
  
  cat("Excel file created for", state_name, ":", file_name, "\n")
  return(list(bouquet = od_bq_rpt, alacarte = od_al_rpt))
}

# --- PROCESS ALL STATES ---
# Generate state summaries
all_summaries <- list()
for(state in final_states) {
  cat("Processing state summary for:", state, "\n")
  summary <- state_summary(main_data_consolidated, state)
  if(!is.null(summary)) {
    all_summaries[[state]] <- summary
  }
}

# Combine all state summaries
if(length(all_summaries) > 0) {
  final_summary <- bind_rows(all_summaries)
} else {
  final_summary <- data.frame()
}

# Generate subscription summaries for each state
for(state in final_states) {
  cat("Processing subscription summary for:", state, "\n")
  tryCatch({
    subs_summary(main_data_consolidated, state, singlepack_7, singlepack_14, singlepack_21, singlepack_28)
  }, error = function(e) {
    cat("Error processing", state, ":", e$message, "\n")
  })
}

# --- CREATE WORD DOCUMENT ---
if(nrow(final_summary) > 0) {
  now_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  doc <- read_docx(path = NULL) %>%
    body_add_par("", style = "Normal")
  
  # Create header
  header_text <- paste0(
    "Report : IPTV Monthly Subscriber Summary \n",
    "Company : Meghbela Cable And BroadBand Services Private Limited \n",
    "Date : ", now_str
  )
  
  header_df <- data.frame(header = header_text)
  
  header_ft <- flextable(header_df, col_keys = "header") %>%
    set_table_properties(layout = "autofit") %>%
    border_remove() %>%
    border_outer(border = fp_border(width = 2, color = "black")) %>%
    padding(padding = 8) %>%
    fontsize(size = 11) %>%
    align(align = "left", part = "all")
  
  # Format summary table
  summary_ft <- flextable(final_summary) %>%
    theme_booktabs() %>%
    border_outer(border = fp_border(color = "black", width = 2)) %>%
    border_inner_h(border = fp_border(color = "gray")) %>%
    border_inner_v(border = fp_border(color = "gray")) %>%
    fontsize(size = 9) %>%
    autofit()
  
  # Build document
  doc <- doc %>%
    body_add_flextable(header_ft) %>%
    body_add_par("", style = "Normal") %>%
    body_add_par("", style = "Normal") %>%
    body_add_par("", style = "Normal") %>%
    body_add_par("", style = "Normal") %>%
    body_add_flextable(summary_ft)
  
  # Save document
  doc_filename <- sprintf("Output/IPTV_Subscriber_Report_%s_%g.docx",
                          month(today() - months(1), label = TRUE, abbr = FALSE),
                          year(today()))
  
  print(doc, target = doc_filename)
  cat("Word document created:", doc_filename, "\n")
}

cat("Processing complete!")