library(tidyverse)
library(dplyr)
library(lubridate)
library(janitor)
library(httr)
library(xlsx)


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
