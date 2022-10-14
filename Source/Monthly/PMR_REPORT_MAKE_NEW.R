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
bouquet_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=1XvbGWeTDsxEvcvLFH1kFjPFBfSA-lP9K&export=download"))
trai_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=1I8UNHSbOoPBvQQ37QgVbMqBqEos10mtL&export=download"),encoding = "UTF-8")

#For Bronze Basic Channels
basic_bouquet = list_bouquet_dated %>% filter(Bouquet=="Bronze Basic") %>% select(Customer.Number,Bouquet,Plan.Name) %>% unique()
Bronze_pivot = basic_bouquet %>% group_by(Plan.Name,Bouquet) %>% summarize(Active_count = n())
Bronze_merged = merge(Bronze_pivot,DF,by.x = "Bouquet", by.y = "Service.Name" ) %>% select(Plan.Name,Bouquet,Channel,Active_count)
colnames(Bronze_merged)[4] <- "Monthly.Subs.of.the.Channel"
Bronze_pivot = Bronze_merged %>% group_by(Channel) %>% summarize(Active_count = sum(Monthly.Subs.of.the.Channel))
Bronze_pivot = merge(Bronze_pivot,trai_names) %>% relocate(TRAI.name, .after = Channel)
write.csv(Bronze_pivot,"Output/FTA_Channels.csv",row.names = F)

#without bronze
bq_try = list_bouquet_dated %>% group_by(Plan.Name,Bouquet) %>% select(Customer.Number,Bouquet,Plan.Name,) %>% unique() %>% summarize(Active_count = n()) %>% filter(!(Bouquet == "Bronze Basic"))
Bouquet_merged = merge(bq_try,DF,by.x = "Bouquet", by.y = "Service.Name" ) %>% select(Plan.Name,Bouquet,Channel,Active_count)
colnames(Bouquet_merged)[4] <- "Monthly.Subs.of.the.Channel"

bc_bouquet_filtered = filter(Bouquet_merged, Plan.Name %in% plan_names$Plan.Name)
bc_bouquet_filtered_bq = filter(bc_bouquet_filtered, Bouquet %in% bouquet_names$Bouquet)
bc_bouquet_filtered_bq = add_column(bc_bouquet_filtered_bq, PackType ='DPO Pack with broadcaster bouquets',.after = 2)

bc_bouquet_filtered_al = filter(bc_bouquet_filtered, !(Bouquet %in% bouquet_names$Bouquet))
bc_bouquet_filtered_al = add_column(bc_bouquet_filtered_al, PackType ='DPO Pack with Alacarte',.after = 2)

bc_bouquet_filtered_noDPO = filter(Bouquet_merged, !(Plan.Name %in% plan_names$Plan.Name))
bc_bouquet_filtered_noDPO = filter(bc_bouquet_filtered_noDPO, !(Plan.Name %in% c('DD Channels','Platinum Digital Postpaid')))
bc_bouquet_filtered_noDPO = add_column(bc_bouquet_filtered_noDPO, PackType ='Broadcaster Bouquets',.after = 2)

bC_bouqet_final = rbind(bc_bouquet_filtered_bq,bc_bouquet_filtered_al,bc_bouquet_filtered_noDPO)
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
all_bouquet = filter(bq_try, Bouquet %in% bouquet_names$Bouquet)
all_bouquet_pivot = all_bouquet %>% group_by(Bouquet) %>% summarize(Total = sum(Active_count))
write.csv(all_bouquet_pivot,"Output/Bouqet_count.csv")

all_dpopack = list_bouquet_dated %>% select(Customer.Number,Bouquet,Plan.Name) %>% unique() %>% filter(Bouquet=="Bronze Basic") %>%
  group_by(Plan.Name,Bouquet) %>% summarize(Active_count = n()) %>% 
  filter(Plan.Name %in% plan_names$Plan.Name)  %>% select(Plan.Name,Active_count) %>% unique() %>%
  group_by(Plan.Name) %>% summarize(Total = sum(Active_count))

write.csv(all_dpopack,"Output/DPO_count.csv")
