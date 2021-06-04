library(tidyverse)
library(dplyr)
library(readxl)
library(RCurl)

bC_bouqet_raw = read_excel(choose.files(default = "_STATE_BOUQUET_RPT",caption = "Select Broadcaster report Bouquet File",multi = FALSE,), skip = 3)
bc_alacarte_raw = read_excel(choose.files(default = "_STATE_ALACARTE_RPT",caption = "Select Broadcaster report Alacarte File",multi = FALSE,), skip = 3)
#LOCATION - SUPPORT GOOGLE DRIVE -> mEGHBELA bILLS AND REPORTS - >PMR support files
plan_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=1WpHeGDD6syAEuEwrSil8CIewbjZweLHb&export=download"))
bouquet_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=1iHErLr_cL36BWzYwsQjOId-YlWWWbAr1&export=download"))
trai_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=12DD_vNDdVqrObI59WalTL_F9cje50Gja&export=download"),encoding = "UTF-8")

names(bC_bouqet_raw) = make.names(names(bC_bouqet_raw))
names(bc_alacarte_raw) = make.names(names(bc_alacarte_raw))
bC_bouqet_raw = filter(bC_bouqet_raw, (Broadcaster.Name != "" & Broadcaster.Name != "Broadcaster Name"))
bc_alacarte_raw = filter(bc_alacarte_raw, (Broadcaster.Name != "" & Broadcaster.Name != "Broadcaster Name"))

#####Work on Bouquet Report
bc_bouquet_filtered = filter(bC_bouqet_raw, Plan.Name %in% plan_names$Plan.Name)
bc_bouquet_filtered_bq = filter(bc_bouquet_filtered, Bouquet %in% bouquet_names$Bouquet)
bc_bouquet_filtered_bq = add_column(bc_bouquet_filtered_bq, PackType ='DPO Pack with broadcaster bouquets',.after = 3)

bc_bouquet_filtered_al = filter(bc_bouquet_filtered, !(Bouquet %in% bouquet_names$Bouquet))
bc_bouquet_filtered_al = add_column(bc_bouquet_filtered_al, PackType ='DPO Pack with Alacarte',.after = 3)

bc_bouquet_filtered_noDPO = filter(bC_bouqet_raw, !(Plan.Name %in% plan_names$Plan.Name))
bc_bouquet_filtered_noDPO = filter(bc_bouquet_filtered_noDPO, !(Plan.Name %in% c('DD Channels','Platinum Digital Postpaid')))
bc_bouquet_filtered_noDPO = add_column(bc_bouquet_filtered_noDPO, PackType ='Broadcaster Bouquets',.after = 3)

bC_bouqet_final = rbind(bc_bouquet_filtered_bq,bc_bouquet_filtered_al,bc_bouquet_filtered_noDPO)
bC_bouqet_final$Monthly.Subs.of.the.Channel = as.numeric(bC_bouqet_final$Monthly.Subs.of.the.Channel)
bC_bouqet_final_pivot = bC_bouqet_final %>% group_by(Channel,PackType) %>% summarize(TotalMonthlySubs = sum(Monthly.Subs.of.the.Channel)) %>%
  pivot_wider(names_from = PackType,values_from = TotalMonthlySubs)
bc_bq_final_pvt_trai = merge(bC_bouqet_final_pivot,trai_names) %>% relocate(TRAI.name, .after = Channel) %>% relocate("DPO Pack with Alacarte", .after = "Broadcaster Bouquets")
bc_bq_final_pvt_trai[is.na(bc_bq_final_pvt_trai)] <- 0

####Work on alacarte report
bc_alacarte_raw$Monthly.Subs.of.the.Channel = as.numeric(bc_alacarte_raw$Monthly.Subs.of.the.Channel) 
bc_alacarte_pivot = bc_alacarte_raw %>% group_by(Channel) %>% summarize(TotalMonthlySubs = sum(Monthly.Subs.of.the.Channel))
bc_ala_pvt_trai = merge(bc_alacarte_pivot,trai_names) %>% relocate(TRAI.name, .after = Channel) ## move trai column before data


write.csv(bc_bq_final_pvt_trai, "Broadcaster Bouquet report PMR.csv", row.names = F)
write.csv(bc_ala_pvt_trai, "Broadcaster Alacarte report PMR.csv", row.names = F)
