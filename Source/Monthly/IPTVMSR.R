library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(flextable)
library(officer)
library(xlsx)

wb_file <- file.choose() ##upload IPTV monthly subscription selecting state
up_file <- file.choose()
iptv_pack <- read.csv(file.choose(),skip = 1) ##upload iptv pacakgewise details
iptv_pack_code <- iptv_pack %>% select(package_name,Plan.Code) %>% unique()
colnames(iptv_pack_code) <- c('Plan_Name','Code')

singlepack_7 = read.csv(file.choose())
singlepack_14 = read.csv(file.choose())
singlepack_21 = read.csv(file.choose())
singlepack_28 = read.csv(file.choose())

# --- LOAD AND CLEAN DATA FOR UNIQUE SUBSCRIBER ---
wb_data <- read_excel(wb_file, sheet = 1, skip = 2)
up_data <- read_excel(up_file, sheet = 1, skip = 2)

wb_summary <- state_summary(wb_data, "WEST BENGAL")
up_summary <- state_summary(up_data, "UTTAR PRADESH")

state_summary <- function(df,state_label){
  # Rename columns manually
  colnames(df) <- c(
    "Plan_Name", "Date", "Account_No", "Subscriber", "Mobile_No",
    "STB_No", "VC_Number", "User_ID", "From_Date", "To_Date"
  )
  # Convert date columns
  df <- df %>%
    mutate(
      Date = as.Date(Date),
      From_Date = as.Date(From_Date),
      To_Date = as.Date(To_Date),
      Plan_Type = ifelse(grepl("HD", Plan_Name, ignore.case = TRUE), "HD", "SD")
    )
  
  # --- GROUP & SUMMARIZE ---
  # Count unique User_IDs per date and plan type
  counts <- df %>%
    group_by(Date, Plan_Type) %>%
    summarise(User_Count = n_distinct(User_ID), .groups = "drop") %>%
    pivot_wider(
      names_from = Plan_Type,
      values_from = User_Count,
      values_fill = list(User_Count = 0)  # <- fill missing HD or SD with 0
    ) %>%
    mutate(
      HD = if (!"HD" %in% names(.)) 0 else HD,
      SD = if (!"SD" %in% names(.)) 0 else SD,
      `HD+SD` = HD + SD
    )
  
  # Create final summary table
  summary_table <- tibble(
    Type = c(
      "Total Unique Active Paying Subscribers (HD+SD)",
      "Total Unique Active Paying Subscribers (SD)"
    )
  )
  
  # Add dynamic date columns
  for (d in sort(unique(counts$Date))) {
    date_col <- format(as.Date(d), "%Y-%m-%d")
    hd_sd <- counts %>% filter(Date == d) %>% pull(`HD`)
    sd_only <- counts %>% filter(Date == d) %>% pull(SD)
    
    summary_table[[date_col]] <- c(hd_sd, sd_only)
  }
  
  # Add Average
  summary_table <- summary_table %>%
    rowwise() %>%
    mutate(Average = mean(c_across(-Type), na.rm = TRUE)) %>%
    ungroup()
  #add state
  summary_table <- summary_table %>%
    mutate(STATE = state_label) %>%
    select(STATE, everything())
  return(summary_table)
}

final_summary <- bind_rows(wb_summary, up_summary)

####CREATE SUBSCRIPTION####
subs_summary <- function(df_raw,state_label,iptv_pack_code,singlepack_7,singlepack_14,singlepack_21,singlepack_28){
  
  colnames(df_raw) <- c(
    "Plan_Name", "Date", "Account_No", "Subscriber", "Mobile_No",
    "STB_No", "VC_Number", "User_ID", "From_Date", "To_Date"
  )
  
  # Ensure Date column is proper Date type
  df_raw <- df_raw %>% mutate(Date = as.Date(Date))
  
  # Get 7th, 14th, 21th, 28th from the Date column
  target_dates <- df_raw %>%
    filter(format(Date, "%d") %in% c("07", "14", "21", "28")) %>%
    pull(Date) %>%
    unique() %>%
    sort()
  
  # Generate dynamic labels
  day_parts <- format(target_dates, "%d")
  day_labels <- paste0("No.of.Subs.On.", as.integer(day_parts), "th.Day")
  names(day_labels) <- as.character(target_dates)  # map for recoding
  
  # Merge with plan code
  df_joined <- df_raw %>%
    left_join(iptv_pack_code, by = "Plan_Name")
  
  summary_table <- df_joined %>%
    group_by(Plan_Name, Code, Date) %>%
    summarise(Unique_Subs = n_distinct(User_ID), .groups = "drop") %>%
    mutate(Date_Label = day_labels[as.character(Date)]) %>%
    select(-Date) %>%
    pivot_wider(names_from = Date_Label, values_from = Unique_Subs, values_fill = 0)
  
  iptvreport = summary_table %>% group_by(Code) %>%
    summarise(across(contains("Subs"), \(x) sum(x, na.rm = TRUE)))
  
  iptv_nw_7 = iptvreport %>% select(Code,No.of.Subs.On.7th.Day) %>% unique()
  iptv_nw_14 = iptvreport %>% select(Code,No.of.Subs.On.14th.Day) %>% unique()
  iptv_nw_21 = iptvreport %>% select(Code,No.of.Subs.On.21th.Day) %>% unique()
  iptv_nw_28 = iptvreport %>% select(Code,No.of.Subs.On.28th.Day) %>% unique()
  
  
  iptv_nw_7_pk = merge(iptv_nw_7,singlepack_7,all.y = T) %>% unique() %>% unite(combined, c('Code','Bouquet'),sep = "|")
  iptv_nw_14_pk = merge(iptv_nw_14,singlepack_14,all.y = F) %>% unique() %>% unite(combined, c('Code','Bouquet'),sep = "|")
  iptv_nw_21_pk = merge(iptv_nw_21,singlepack_21,all.y = F) %>% unique() %>% unite(combined, c('Code','Bouquet'),sep = "|")
  iptv_nw_28_pk = merge(iptv_nw_28,singlepack_28,all.y = F) %>% unique() %>% unite(combined, c('Code','Bouquet'),sep = "|")
  iptv_combo = merge(iptv_nw_7_pk,iptv_nw_14_pk, by.x = "combined", by.y = "combined", all = T)
  iptv_combo = merge(iptv_combo,iptv_nw_21_pk, by.x = "combined", by.y = "combined", all = T)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
  iptv_combo = merge(iptv_combo, iptv_nw_28_pk, all = T) %>% separate(combined, into = c("Code","Bouquet"),sep = "\\|")
  iptv_combo[is.na(iptv_combo)] <- 0
  iptv_combo$No.of.Subs.On.7th.Day = as.numeric(iptv_combo$No.of.Subs.On.7th.Day)
  iptv_combo$No.of.Subs.On.14th.Day = as.numeric(iptv_combo$No.of.Subs.On.14th.Day)
  iptv_combo$No.of.Subs.On.21th.Day = as.numeric(iptv_combo$No.of.Subs.On.21th.Day)
  iptv_combo$No.of.Subs.On.28th.Day = as.numeric(iptv_combo$No.of.Subs.On.28th.Day)
  iptv_combo = iptv_combo %>% mutate(Monthly.Subs.of.the.Channel = rowMeans(select(iptv_combo, starts_with("No.of"))))
  
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
  
  ##NTO report all
  write.xlsx(as.data.frame(od_bq_rpt), file=sprintf("Output/%s_IPTV_MSR__all__%s_%g.xlsx",state_label,month(today() - months(1),label = TRUE, abbr = F),year(today())), sheetName="Bouquet", row.names=FALSE)
  write.xlsx(as.data.frame(od_al_rpt), file=sprintf("Output/%s_IPTV_MSR__all__%s_%g.xlsx",state_label,month(today() - months(1),label = TRUE, abbr = F),year(today())), sheetName="Alacarte", append=TRUE, row.names=FALSE)
  
}


wb_subssummary <- subs_summary(wb_data, "WEST BENGAL",iptv_pack_code,singlepack_7,singlepack_14,singlepack_21,singlepack_28)

up_subssummary <- subs_summary(up_data, "UTTAR PRADESH",iptv_pack_code,singlepack_7,singlepack_14,singlepack_21,singlepack_28)







####print word file####
# --- Create Document in Landscape Orientation ---
doc <- read_docx(path = NULL) %>%
  body_add_par("", style = "Normal")

# Apply section at the end
#doc <- body_end_section(doc, value = sect_properties)

# Create the header as a mini table (to apply borders)
header_text  = paste0(
  "Report : IPTV Monthly Subscriber Summary \n",
  "Company : Meghbela Cable And BroadBand Services Private Limited \n",
  "Date : ", now_str
)
# Create a 1-column data frame to render as a flextable box
header_df <- data.frame(header = header_text)

header_ft <- flextable(header_df, col_keys = "header") %>%
  set_table_properties(layout = "autofit") %>%
  border_remove() %>%
  border_outer(border = fp_border(width = 2, color = "black")) %>%
  padding(padding = 8) %>%
  fontsize(size = 11) %>%
  align(align = "left", part = "all")

# --- Step 3: Format the summary table with gridlines ---
summary_ft <- flextable(final_summary) %>%
  theme_booktabs() %>%
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(border = fp_border(color = "gray")) %>%
  border_inner_v(border = fp_border(color = "gray")) %>%
  fontsize(size = 9) %>%
  autofit()

# --- Step 4: Build the document ---
doc <- doc %>%
  body_add_flextable(header_ft) %>%
  body_add_par("", style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(summary_ft)

# --- Step 5: Save DOCX ---
print(doc, target = sprintf("Output/IPTV_Subscriber_Report_%s_%g.docx",month(today() - months(1),label = TRUE, abbr = F),year(today())))
