library(tidyverse)
library(dplyr)
library(readxl)
library(purrr)

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

####For Quarterly report
all_bouquet = filter(bq_try, !(Plan.Name %in% plan_names$Plan.Name))
all_bouquet_pivot = all_bouquet %>% group_by(Plan.Name) %>% summarize(Total = sum(Active_count))
write.csv(all_bouquet_pivot,"Output/Bouqet_count.csv")

all_dpopack = filter(bq_try, (Plan.Name %in% plan_names$Plan.Name)) %>%
  group_by(Plan.Name) %>% summarize(Total = sum(Active_count))

write.csv(all_dpopack,"Output/DPO_count.csv")

###for mcbs old company
##############
custom_num = list_bouquet_dated %>% select(Cust.Id) %>% unique()
set.seed(123)
total_rows <- nrow(custom_num)
random_indices <- sample(total_rows, 1000)
subset_dataset <- custom_num[random_indices, ]
subset_dataframe <- data.frame(subset_dataset)
colnames(subset_dataframe)[1] <- "Cust.Id"
bq_try_mcbs = merge(list_bouquet_dated,subset_dataframe)
bq_try_mcbs = bq_try_mcbs %>% group_by(Plan.Name,CHANNEL_NAME_5) %>% select(Cust.Id,CHANNEL_NAME_5,Plan.Name,) %>% unique() %>% summarize(Active_count = n()) %>% filter(!(CHANNEL_NAME_5 == "Bronze Basic"))

all_bouquet = filter(bq_try_mcbs, !(Plan.Name %in% plan_names$Plan.Name))
all_bouquet_pivot = all_bouquet %>% group_by(Plan.Name) %>% summarize(Total = sum(Active_count))
write.csv(all_bouquet_pivot,"Output/mcbs_Bouqet_count.csv")

all_dpopack = filter(bq_try_mcbs, (Plan.Name %in% plan_names$Plan.Name)) %>%
  group_by(Plan.Name) %>% summarize(Total = sum(Active_count))

write.csv(all_dpopack,"Output/mcbs_DPO_count.csv")





