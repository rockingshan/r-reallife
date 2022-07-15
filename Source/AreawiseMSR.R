library(tidyverse)
library(dplyr)
library(lubridate)
library(janitor)
library(httr)
library(xlsx)
library(readxl)


DF1 = read.csv(file.choose(new = F)) #pack details
DF2 = read.csv(file.choose(new = F)) #service details
DF = merge(DF2,DF1)
cb = c("Free to AIR","DD","Republic TV")
DF = filter(DF, !(Broadcaster %in% cb))
DF = DF %>% select(Service.Name,Channel) %>% unique()
plan_names = read.csv(sprintf("https://spreadsheets.google.com/feeds/download/spreadsheets/Export?key=17fLf3_5nMKuOZxMvKY_baJjD3G8l-KKHxw3WSTNKh6o&exportFormat=csv"))
bouquet_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=1iHErLr_cL36BWzYwsQjOId-YlWWWbAr1&export=download"))
lco_area = read.csv(file.choose(new = F))

list_bouquet_dated = read.csv(file.choose(new = F)) #import MQ data bouquet
list_alacarte = read.csv(file.choose(new = F)) #import MQ alacarte details

#alacarte work
alacarte_base_fl = list_alacarte %>% select(Customer.Number,Channel.Name,Broadcaster.Name,Month,Week,LCO.Code) #%>% unique()
ala_frm_bouq_rp = filter(list_bouquet_dated, !(Bouquet %in% bouquet_names$Bouquet)) %>% filter(!(Bouquet == "Bronze Basic")) %>% select(Customer.Number,Bouquet,Broadcaster.Name,Month,Week,LCO.Code)
ala_frm_bouq_rp_chnl = merge(ala_frm_bouq_rp,DF,by.x = "Bouquet",by.y = "Service.Name",all.x = T)
ala_frm_bouq_rp_chnl_cln = na.omit(ala_frm_bouq_rp_chnl)
colnames(ala_frm_bouq_rp_chnl_cln)[7] <- 'Channel.Name'
ala_from_bq_report = ala_frm_bouq_rp_chnl_cln %>% select(Customer.Number,Channel.Name,Broadcaster.Name,Month,Week,LCO.Code)  #%>% unique()
alacarte_total = rbind(alacarte_base_fl,ala_from_bq_report)
remove(alacarte_base_fl,ala_frm_bouq_rp,ala_frm_bouq_rp_chnl,ala_frm_bouq_rp_chnl_cln,ala_from_bq_report) #remove objects after calculation
alacarte_total = filter(alacarte_total, !(Broadcaster.Name %in% cb))
alacarte_pivot = alacarte_total %>% group_by(LCO.Code,Channel.Name,Week,Broadcaster.Name) %>% summarize(WeeklySubs = n()) %>% 
  pivot_wider(names_from = Week,values_from = WeeklySubs)
alacarte_report = merge(alacarte_pivot,lco_area)
#alacarte_report = alacarte_report %>% mutate()
write.csv(alacarte_report, "Alacarte_lcowise.csv", row.names = F)

####bouquet report
bouquet_data_fl = filter(list_bouquet_dated, (Bouquet %in% bouquet_names$Bouquet)) %>% filter(!(Bouquet == "Bronze Basic")) %>% select(Customer.Number,Bouquet,Broadcaster.Name,Week,LCO.Code)
bouquet_pivot = bouquet_data_fl %>% group_by(LCO.Code,Bouquet,Week,Broadcaster.Name) %>% summarize(WeeklySubs = n()) %>% 
  pivot_wider(names_from = Week,values_from = WeeklySubs)
bouquet_report = merge(bouquet_pivot,lco_area)
write.csv(bouquet_report, "Bouquet_lcowise.csv",row.names = F)


#######################LCOWISE MSR REPORT PROPER
BQ_REPORT = read_xls(file.choose(new = F),skip = 1)
AL_REPORT = read_xls(file.choose(new = F),skip = 1)
names(BQ_REPORT) <- make.names(names(BQ_REPORT))
names(AL_REPORT) <- make.names(names(AL_REPORT))
hdnd_nm = c('MDKH','MDBKT','MDBQA','MDCNDP','MDDHH','MDHCNJV','MDOR','MDSKWJV','TESTENTITY')
br_rm = c('ABP News Network Pvt Limited','Free to AIR','DD','Discontinued Channel','Republic TV')
pln_rm = c('DPO Promotional Bundle','Discontinued Package @ No Renew')

bq_rp_clr = filter(BQ_REPORT, !(Entity.Code %in% hdnd_nm))
bq_rp_clr = filter(bq_rp_clr, !(Broadcaster.Name %in% br_rm))
bq_rp_clr = filter(bq_rp_clr, !(Plan.Name %in% pln_rm))

al_rp_clr = filter(AL_REPORT, !(Entity.Code %in% hdnd_nm))
al_rp_clr = filter(al_rp_clr, !(Broadcaster.Name %in% br_rm))
al_rp_clr = filter(al_rp_clr, !(Plan.Name %in% pln_rm))

dpo_plan_al = filter(AL_REPORT, Plan.Name == 'DPO Promotional Bundle')
colnames(dpo_plan_al)[4] <- "Bouquet"
dpo_plan_bq = filter(BQ_REPORT, Plan.Name == 'DPO Promotional Bundle')
dpo_plan = rbind(dpo_plan_bq,dpo_plan_al)

write.csv(bq_rp_clr, "Output/Bouquet_Report_LCOWISE.csv", row.names = F)
write.csv(al_rp_clr, "Output/Alacarte_Report_LCOWISE.csv", row.names = F)
write.csv(dpo_plan, "Output/DPO_PROMOTIONAL_Plan_LCOWISE.csv", row.names = F)



##############make plan count
list_bouquet_ = read.csv(file.choose(new = F), header = F)[,17:26]
colnames(list_bouquet_) <-c("Broadcaster","Plan","Bouquet","Channel_count","Channels","7th","14th","21st","28th","Monthly_average")
list_bq_flt = filter(list_bouquet_, !(Broadcaster %in% br_rm))
list_bq_flt$Monthly_average <- gsub(",","",list_bq_flt$Monthly_average)
list_bq_flt$Monthly_average <- as.numeric(list_bq_flt$Monthly_average)
list_bq_pvt = list_bq_flt %>% group_by(Plan,Bouquet) %>% summarise(Subs_count = sum(Monthly_average)) %>%
  group_by(Plan) %>% summarise(Max_count = max(Subs_count))
write.csv(list_bq_pvt,"march.csv",row.names = F)


#####COMBINE BOTH REPORT AND FILTER AS A BROADCASTER
colnames(al_rp_clr)[4] <- "Bouquet"
comboReport = rbind(bq_rp_clr,al_rp_clr)
comboReportFilter = filter(comboReport, Broadcaster.Name == "Star India Pvt. Ltd.")
comboReportPivot = comboReportFilter %>% group_by(Plan.Name,Bouquet) %>% summarise(Monthly_average = sum(Monthly.Subs.of.the.Channel))
comboReportPivot1 = comboReportPivot %>% group_by(Plan.Name) %>% summarise(Plan_count = max(Monthly_average))
write.csv(comboReportPivot1,"Plan_counts.csv",row.names = F)
write.csv(comboReportPivot, "STAR_India_combo_report.csv",row.names = F)
