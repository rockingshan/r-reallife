library(tidyverse)
library(dplyr)
library(readxl)
library(purrr)
library(officer)
library(flextable)
library(openxlsx)
library(lubridate)

list_bouquet_dated = read.csv(file.choose(new = F)) #import MQ data bouquet
list_alacarte = read.csv(file.choose(new = F)) #import MQ alacarte details
DF1 = read.csv(file.choose(new = F)) #pack details
DF2 = read.csv(file.choose(new = F)) #plan config with services, MAKE ONLY 30
DF2 = DF2 %>% filter(Billing.Frequency=="30D")
DF = merge(DF2,DF1)
DF = DF %>% select(Service.Name,Channel) %>% unique()

plan_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=17GoiwT4nWCn0J_7HJF0ZyL5Y0-JPNwOJ&export=download"))
bouquet_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=1INvieYzh6sc2O9XsMHVLI2d2LEOeuYT_&export=download"))
trai_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=1I8UNHSbOoPBvQQ37QgVbMqBqEos10mtL&export=download"),encoding = "UTF-8")

###OLD FIND
# OLD_PLAN = read.csv(sprintf("https://drive.google.com/u/0/uc?id=1i7EBGH1mzd8Xmy2ZAOLGJAVbXCYuRByz&export=download"))
# bc_bouquet_filtered = filter(list_bouquet_dated, Plan.Name %in% OLD_PLAN$Plan.Name) %>% filter(!(CHANNEL_NAME_5 %in% c("Meghbela Bonanza @ 330","Meghbela Basic Pack @ 155")))
# write.csv(bc_bouquet_filtered,"OldCustomer.csv")

#For Bronze Basic Channels
basic_bouquet = list_bouquet_dated %>% filter(CHANNEL_NAME_5=="Bronze Basic") %>% select(Cust.Id,CHANNEL_NAME_5,Plan.Name) %>% unique()
Bronze_pivot = basic_bouquet %>% group_by(Plan.Name,CHANNEL_NAME_5) %>% summarize(Active_count = n())
Bronze_merged = merge(Bronze_pivot,DF,by.x = "CHANNEL_NAME_5", by.y = "Service.Name" ) %>% select(Plan.Name,CHANNEL_NAME_5,Channel,Active_count)
colnames(Bronze_merged)[4] <- "Monthly.Subs.of.the.Channel"
Bronze_pivot = Bronze_merged %>% group_by(Channel) %>% summarize(Active_count = sum(Monthly.Subs.of.the.Channel))
Bronze_pivot = merge(Bronze_pivot,trai_names) %>% relocate(TRAI.name, .after = Channel)
write.csv(Bronze_pivot,"Output/FTA_Channels.csv",row.names = F)



#without bronze
bq_try = list_bouquet_dated %>% group_by(Plan.Name,CHANNEL_NAME_5) %>% select(Cust.Id,CHANNEL_NAME_5,Plan.Name,) %>% unique() %>% summarize(Active_count = n()) %>% filter(!(CHANNEL_NAME_5 == "Bronze Basic"))
Bouquet_merged = merge(bq_try,DF,by.x = "CHANNEL_NAME_5", by.y = "Service.Name" ) %>% select(Plan.Name,CHANNEL_NAME_5,Channel,Active_count)
colnames(Bouquet_merged)[4] <- "Monthly.Subs.of.the.Channel"


bc_bouquet_filtered = filter(bq_try, Plan.Name %in% plan_names$Plan.Name) %>% select(Plan.Name,Active_count)
####calculate new pacakge
plan_confg = read.csv(file.choose()) ##choose single pack config
bc_dpo_spread= merge(bc_bouquet_filtered,plan_confg)
bc_dpo_bouq = bc_dpo_spread %>% filter(X == "Bouquet")
bc_dpo_bouq_merged = merge(bc_dpo_bouq,DF,by.x = "Bouquet", by.y = "Service.Name") %>% select(Plan.Name,Bouquet,Channel,Active_count)
colnames(bc_dpo_bouq_merged)[4] <- "Monthly.Subs.of.the.Channel"

#bc_bouquet_filtered_bq = filter(bc_dpo_bouq_merged, Bouquet %in% bouquet_names$Bouquet)
bc_dpo_bouq_merged = add_column(bc_dpo_bouq_merged, PackType ='DPO Pack with broadcaster bouquets',.after = 2)

bc_bouquet_filtered_al = bc_dpo_spread %>% filter(X == "Alacarte") %>% select(Plan.Name,Bouquet,Active_count)
bc_bouquet_filtered_al = add_column(bc_bouquet_filtered_al, PackType ='DPO Pack with Alacarte',.after = 2)
bc_bouquet_filtered_al = bc_bouquet_filtered_al %>% mutate(Channel = Bouquet,.after = 3)
#colnames(bc_bouquet_filtered_al)[2] <- "Channel"
colnames(bc_bouquet_filtered_al)[5] <- "Monthly.Subs.of.the.Channel"

bc_bouquet_filtered_noDPO = filter(Bouquet_merged, !(Plan.Name %in% plan_names$Plan.Name))
bc_bouquet_filtered_noDPO = filter(bc_bouquet_filtered_noDPO, !(Plan.Name %in% c('DD Channels','Platinum Digital Postpaid')))
bc_bouquet_filtered_noDPO = add_column(bc_bouquet_filtered_noDPO, PackType ='Broadcaster Bouquets',.after = 2)
colnames(bc_bouquet_filtered_noDPO)[2] <- "Bouquet"

bC_bouqet_final = rbind(bc_dpo_bouq_merged,bc_bouquet_filtered_al,bc_bouquet_filtered_noDPO)
bC_bouqet_final$Monthly.Subs.of.the.Channel = as.numeric(bC_bouqet_final$Monthly.Subs.of.the.Channel)
bC_bouqet_final_pivot = bC_bouqet_final %>% group_by(Channel,PackType) %>% summarize(TotalMonthlySubs = sum(Monthly.Subs.of.the.Channel)) %>%
  pivot_wider(names_from = PackType,values_from = TotalMonthlySubs)
bc_bq_final_pvt_trai = merge(bC_bouqet_final_pivot,trai_names) %>% relocate(TRAI.name, .after = Channel) %>% relocate("DPO Pack with Alacarte", .after = "Broadcaster Bouquets")
bc_bq_final_pvt_trai[is.na(bc_bq_final_pvt_trai)] <- 0

####alacarte
ala_count = list_alacarte %>% group_by(Channel.Name) %>% summarise(Monthly.Subs.of.the.Channel=n())
colnames(ala_count)[1] <- "Channel"
bc_ala_pvt_trai = merge(ala_count,trai_names) %>% relocate(TRAI.name, .after = Channel) ## move trai column before data
write.csv(bc_bq_final_pvt_trai, "Output/Broadcaster Bouquet report PMR.csv", row.names = F)
write.csv(bc_ala_pvt_trai, "Output/Broadcaster Alacarte report PMR.csv", row.names = F)

bq_subs = list_bouquet_dated %>% select(Cust.Id,Lco.Code) %>% unique()
ala_subs = list_alacarte %>% select(Cust.Id,Lco.Code) %>% unique()
all_sub = rbind(bq_subs,ala_subs) %>% unique() 
tibble(all_sub)

####For Quarterly report
all_bouquet = filter(bq_try, !(Plan.Name %in% plan_names$Plan.Name))
all_bouquet_pivot = all_bouquet %>% group_by(Plan.Name) %>% summarize(Total = sum(Active_count))
write.csv(all_bouquet_pivot,"Output/Bouqet_count.csv")

all_dpopack = filter(bq_try, (Plan.Name %in% plan_names$Plan.Name)) %>%
  group_by(Plan.Name) %>% summarize(Total = sum(Active_count))

write.csv(all_dpopack,"Output/DPO_count.csv")


####For Berhampore#####


list_bouquet_dated = list_bouquet_dated %>% filter(grepl("^MSW", Lco.Code))#filter berhampore data
list_alacarte = list_alacarte %>% filter(grepl("^MSW", Lco.Code)) #filter berhampore data


#For Bronze Basic Channels
basic_bouquet = list_bouquet_dated %>% filter(CHANNEL_NAME_5=="Bronze Basic") %>% select(Cust.Id,CHANNEL_NAME_5,Plan.Name) %>% unique()
Bronze_pivot = basic_bouquet %>% group_by(Plan.Name,CHANNEL_NAME_5) %>% summarize(Active_count = n())
Bronze_merged = merge(Bronze_pivot,DF,by.x = "CHANNEL_NAME_5", by.y = "Service.Name" ) %>% select(Plan.Name,CHANNEL_NAME_5,Channel,Active_count)
colnames(Bronze_merged)[4] <- "Monthly.Subs.of.the.Channel"
Bronze_pivot = Bronze_merged %>% group_by(Channel) %>% summarize(Active_count = sum(Monthly.Subs.of.the.Channel))
Bronze_pivot = merge(Bronze_pivot,trai_names) %>% relocate(TRAI.name, .after = Channel)
write.csv(Bronze_pivot,"Output/Berhampore_FTA_Channels.csv",row.names = F)




bq_try = list_bouquet_dated %>% group_by(Plan.Name,CHANNEL_NAME_5) %>% select(Cust.Id,CHANNEL_NAME_5,Plan.Name,) %>% unique() %>% summarize(Active_count = n()) %>% filter(!(CHANNEL_NAME_5 == "Bronze Basic"))
Bouquet_merged = merge(bq_try,DF,by.x = "CHANNEL_NAME_5", by.y = "Service.Name" ) %>% select(Plan.Name,CHANNEL_NAME_5,Channel,Active_count)
colnames(Bouquet_merged)[4] <- "Monthly.Subs.of.the.Channel"


bc_bouquet_filtered = filter(bq_try, Plan.Name %in% plan_names$Plan.Name) %>% select(Plan.Name,Active_count)
####calculate new pacakge
plan_confg = read.csv(file.choose()) ##choose single pack config
bc_dpo_spread= merge(bc_bouquet_filtered,plan_confg)
bc_dpo_bouq = bc_dpo_spread %>% filter(X == "Bouquet")
bc_dpo_bouq_merged = merge(bc_dpo_bouq,DF,by.x = "Bouquet", by.y = "Service.Name") %>% select(Plan.Name,Bouquet,Channel,Active_count)
colnames(bc_dpo_bouq_merged)[4] <- "Monthly.Subs.of.the.Channel"

#bc_bouquet_filtered_bq = filter(bc_dpo_bouq_merged, Bouquet %in% bouquet_names$Bouquet)
bc_dpo_bouq_merged = add_column(bc_dpo_bouq_merged, PackType ='DPO Pack with broadcaster bouquets',.after = 2)

bc_bouquet_filtered_al = bc_dpo_spread %>% filter(X == "Alacarte") %>% select(Plan.Name,Bouquet,Active_count)
bc_bouquet_filtered_al = add_column(bc_bouquet_filtered_al, PackType ='DPO Pack with Alacarte',.after = 2)
bc_bouquet_filtered_al = bc_bouquet_filtered_al %>% mutate(Channel = Bouquet,.after = 3)
#colnames(bc_bouquet_filtered_al)[2] <- "Channel"
colnames(bc_bouquet_filtered_al)[5] <- "Monthly.Subs.of.the.Channel"

bc_bouquet_filtered_noDPO = filter(Bouquet_merged, !(Plan.Name %in% plan_names$Plan.Name))
bc_bouquet_filtered_noDPO = filter(bc_bouquet_filtered_noDPO, !(Plan.Name %in% c('DD Channels','Platinum Digital Postpaid')))
bc_bouquet_filtered_noDPO = add_column(bc_bouquet_filtered_noDPO, PackType ='Broadcaster Bouquets',.after = 2)
colnames(bc_bouquet_filtered_noDPO)[2] <- "Bouquet"

bC_bouqet_final = rbind(bc_dpo_bouq_merged,bc_bouquet_filtered_al,bc_bouquet_filtered_noDPO)
bC_bouqet_final$Monthly.Subs.of.the.Channel = as.numeric(bC_bouqet_final$Monthly.Subs.of.the.Channel)
bC_bouqet_final_pivot = bC_bouqet_final %>% group_by(Channel,PackType) %>% summarize(TotalMonthlySubs = sum(Monthly.Subs.of.the.Channel)) %>%
  pivot_wider(names_from = PackType,values_from = TotalMonthlySubs)
bc_bq_final_pvt_trai = merge(bC_bouqet_final_pivot,trai_names) %>% relocate(TRAI.name, .after = Channel) %>% relocate("DPO Pack with Alacarte", .after = "Broadcaster Bouquets")
bc_bq_final_pvt_trai[is.na(bc_bq_final_pvt_trai)] <- 0

####alacarte
ala_count = list_alacarte %>% group_by(Channel.Name) %>% summarise(Monthly.Subs.of.the.Channel=n())
colnames(ala_count)[1] <- "Channel"
bc_ala_pvt_trai = merge(ala_count,trai_names) %>% relocate(TRAI.name, .after = Channel) ## move trai column before data
write.csv(bc_bq_final_pvt_trai, "Output/Berhampore_Broadcaster Bouquet report PMR.csv", row.names = F)
write.csv(bc_ala_pvt_trai, "Output/Berhampore_Broadcaster Alacarte report PMR.csv", row.names = F)

bq_subs = list_bouquet_dated %>% select(Cust.Id,Lco.Code) %>% unique()
ala_subs = list_alacarte %>% select(Cust.Id,Lco.Code) %>% unique()
all_sub = rbind(bq_subs,ala_subs) %>% unique() 
tibble(all_sub)
####For Quarterly report
all_bouquet = filter(bq_try, !(Plan.Name %in% plan_names$Plan.Name))
all_bouquet_pivot = all_bouquet %>% group_by(Plan.Name) %>% summarize(Total = sum(Active_count))
write.csv(all_bouquet_pivot,"Output/Berhampore_Bouqet_count.csv")

all_dpopack = filter(bq_try, (Plan.Name %in% plan_names$Plan.Name)) %>%
  group_by(Plan.Name) %>% summarize(Total = sum(Active_count))

write.csv(all_dpopack,"Output/Berhampore_DPO_count.csv")


####FOR IPTV
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

# Get 7th, 14th, 21st, 28th from the Date column
target_dates <- main_data %>%
  filter(format(Date, "%d") %in% c("07", "14", "21", "28")) %>%
  pull(Date) %>%
  unique() %>%
  sort()

# Generate dynamic labels
day_parts <- format(target_dates, "%d")
day_labels <- paste0("No.of.Subs.On.", as.integer(day_parts), "th.Day")
names(day_labels) <- as.character(target_dates)

# Create summary table using Plan_Code directly (no merge needed)
summary_table <- main_data %>%
  group_by(Plan_Name, Plan_Code, Date) %>%
  summarise(Unique_Subs = n_distinct(User_ID), .groups = "drop") %>%
  mutate(Date_Label = day_labels[as.character(Date)]) %>%
  filter(!is.na(Date_Label)) %>%  # Only keep target dates
  select(-Date) %>%
  pivot_wider(names_from = Date_Label, values_from = Unique_Subs, values_fill = 0)

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

####create channels
Bouquet_merged = merge(od_bq_rpt,DF,by.x = "Bouquet", by.y = "Service.Name" ) %>%
  group_by(Channel) %>%
  summarise(Total = sum(Average))
Bouquet_merged_trai = merge(Bouquet_merged,trai_names) %>% relocate(TRAI.name, .after = Channel)
write.csv(Bouquet_merged_trai,"Output/IPTV_DPO_bouquet_count.csv",row.names = F)


Ala_merged_trai = merge(od_al_rpt,trai_names) %>% relocate(TRAI.name, .after = Channel) %>% select(Channel,TRAI.name,Average)
write.csv(Ala_merged_trai,"Output/IPTV_DPO_Alacarte.csv",row.names = F)





