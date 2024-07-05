library(tidyverse)
library(dplyr)
library(xlsx)
library(stringr)

active_to_msr_format <- function(){
  list_active = read.csv(file.choose())
  list_active1 = list_active %>% filter(!(PLAN_CODE == '')) %>% select(CUSTOMER_NBR,CONTRACT_NUMBER,ENTITY_CODE,LCO_CITY,STB,SERVICE_CODE,SERVICE_NAME,PLAN_NAME) %>%
    filter(!(PLAN_NAME == 'DPO Promotional Bundle')) %>% filter(!(PLAN_NAME == 'zDelete Pairing')) %>% filter(!(PLAN_NAME == 'All Channel Special Pack')) %>% unique()
  pack_details = read.csv(file.choose()) %>% select(Service.Code,Broadcaster) %>% unique()
  colnames(pack_details)[1] = "SERVICE_CODE"
  colnames(pack_details)[2] = "Broadcaster.Name"
  colnames(list_active1)[8] = "Plan.Name"
  listActiveBroadcast = merge(list_active1,pack_details,all.x = T) %>% filter(!(is.na(Broadcaster.Name)))
  listActiveCount = listActiveBroadcast %>% group_by(LCO_CITY,Broadcaster.Name,Plan.Name,SERVICE_NAME) %>% summarise(Active.Cust = n())
  
  return(listActiveCount)
}
###Single cas code pack names from gdrive
bouquet_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=1yk7CDbZghpUUZzWmGbmQyz44baVr688s&export=download"))
bouquet_names = bouquet_names %>% add_row(Bouquet = "Bronze Basic")

#singlecode pak
singlepack_7 = read.csv(file.choose())
singlepack_14 = read.csv(file.choose())
singlepack_21 = read.csv(file.choose())
singlepack_28 = read.csv(file.choose())

msrAll7th = active_to_msr_format()
colnames(msrAll7th)[5] <- 'No.of.Subs.On.7th.Day'
msrAll714th = active_to_msr_format()
colnames(msrAll714th)[5] <- 'No.of.Subs.On.14th.Day'
msrAll721th = active_to_msr_format()
colnames(msrAll721th)[5] <- 'No.of.Subs.On.21st.Day'
msrAll28th = active_to_msr_format()
colnames(msrAll28th)[5] <- 'No.of.Subs.On.28th.Day'

msrAllCombo = merge(msrAll7th,msrAll714th, all = T)
msrAllCombo = merge(msrAllCombo,msrAll721th, all = T)
msrAllCombo = merge(msrAllCombo,msrAll28th, all = T)
msrAllCombo[is.na(msrAllCombo)] <- 0
msrAllCombo = msrAllCombo %>% mutate(Monthly.Subs.of.the.Channel = rowMeans(select(msrAllCombo, starts_with("No.of"))))
##
#msrAllCombo = read.csv(file.choose())

msrAlacarte = msrAllCombo %>% filter((Plan.Name %in% c("Alacarte Plan","Punjabi Channel Group","Telegu Channels Group","Odia Channel Group")))
msrAlacarteSub = msrAlacarte %>% filter((SERVICE_NAME %in% c("Sidharth Odia Pack-1","Odisha Tv Bouqet 1")))
msrAlacarte = msrAlacarte %>% filter(!(SERVICE_NAME %in% c("Sidharth Odia Pack-1","Odisha Tv Bouqet 1")))
#colnames(msrAlacarte)[1] <- 'Broadcaster.Name'
#colnames(msrAlacarte)[2] <- 'Plan.Name'
msrBouquet = msrAllCombo %>% filter(!(Plan.Name %in% c("Alacarte Plan","Punjabi Channel Group","Telegu Channels Group","Odia Channel Group")))
msrBouquet = rbind(msrBouquet,msrAlacarteSub)
#colnames(msrBouquet)[1] <- 'Broadcaster.Name'
#colnames(msrBouquet)[2] = "Plan.Name"
colnames(msrBouquet)[4] = "Bouquet"

###service to channel block\
pack = read.csv(file.choose()) %>% select(Service.Code,Channel,Broadcaster) %>% unique()
service = read.csv(file.choose()) %>% select(Service.Code,Service.Name) %>% unique()
serviceChannel = merge(service,pack) %>% select(Service.Name,Channel) %>% unique()
colnames(serviceChannel)[1] = "SERVICE_NAME"

####
msrAlacarte = merge(msrAlacarte,serviceChannel,all.x = T)

####work on Bouqets#######
msrBouquet = msrBouquet %>% filter(!(Broadcaster.Name %in% c("ABP News Network Pvt Limited","Free to AIR","Republic TV","DD")))
msrBouquet_new = msrBouquet %>% filter(Bouquet %in% bouquet_names$Bouquet) %>% select(LCO_CITY,Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day,
                                                                                   No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day,Monthly.Subs.of.the.Channel)
msrBouquet_new_7 = msrBouquet_new %>% select(LCO_CITY,Plan.Name,No.of.Subs.On.7th.Day) %>% unique()
msrBouquet_new_14 = msrBouquet_new %>% select(LCO_CITY,Plan.Name,No.of.Subs.On.14th.Day) %>% unique()
msrBouquet_new_21 = msrBouquet_new %>% select(LCO_CITY,Plan.Name,No.of.Subs.On.21st.Day) %>% unique()
msrBouquet_new_28 = msrBouquet_new %>% select(LCO_CITY,Plan.Name,No.of.Subs.On.28th.Day) %>% unique()
msrBouquet_new_avg = msrBouquet_new %>% select(LCO_CITY,Plan.Name,Monthly.Subs.of.the.Channel) %>% unique()

msrBouquet_new_7_pk = merge(msrBouquet_new_7,singlepack_7,all.y = F) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")
msrBouquet_new_14_pk = merge(msrBouquet_new_14,singlepack_14,all.y = F) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")
msrBouquet_new_21_pk = merge(msrBouquet_new_21,singlepack_21,all.y = F) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")
msrBouquet_new_28_pk = merge(msrBouquet_new_28,singlepack_28,all.y = F) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")

msrBouquet_combo_new = merge(msrBouquet_new_7_pk,msrBouquet_new_14_pk, all = T)
msrBouquet_combo_new = merge(msrBouquet_combo_new, msrBouquet_new_21_pk,all = T)
msrBouquet_combo_new = merge(msrBouquet_combo_new, msrBouquet_new_28_pk, all = T) %>% separate(combined, into = c("Plan.Name","Bouquet"),sep = "\\|")

msrBouquet_combo_new[is.na(msrBouquet_combo_new)] <- 0
msrBouquet_combo_new$No.of.Subs.On.7th.Day = as.numeric(msrBouquet_combo_new$No.of.Subs.On.7th.Day)
msrBouquet_combo_new$No.of.Subs.On.14th.Day = as.numeric(msrBouquet_combo_new$No.of.Subs.On.14th.Day)
msrBouquet_combo_new$No.of.Subs.On.21st.Day = as.numeric(msrBouquet_combo_new$No.of.Subs.On.21st.Day)
msrBouquet_combo_new$No.of.Subs.On.28th.Day = as.numeric(msrBouquet_combo_new$No.of.Subs.On.28th.Day)
msrBouquet_combo_new = msrBouquet_combo_new %>% mutate(Monthly.Subs.of.the.Channel = rowMeans(select(msrBouquet_combo_new, starts_with("No.of"))))

msrBouquet_combo_new_bouq = msrBouquet_combo_new %>% filter(X == 'Bouquet') %>% select(LCO_CITY,Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day,
                                                                             No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day,Monthly.Subs.of.the.Channel)
##calculate old process
msrBouquet_old = msrBouquet %>% filter(!(Bouquet %in% bouquet_names$Bouquet))
msrBouquet_old_filterred = msrBouquet_old %>%
  select(LCO_CITY,Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day,
         No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day,Monthly.Subs.of.the.Channel) %>% distinct() %>% na.omit()
##add single cas code pack
msrBouquet_bq_all = rbind(msrBouquet_old_filterred,msrBouquet_combo_new_bouq)

##all bouqet count
msrBouqRpt = msrBouquet_bq_all %>% 
  group_by(Broadcaster.Name,Bouquet) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day),'Average' = sum(Monthly.Subs.of.the.Channel))

##finding DPO count
msrBouqRptPlan = msrBouquet_bq_all %>% 
  group_by(Broadcaster.Name,Plan.Name,Bouquet) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day),'Average' = sum(Monthly.Subs.of.the.Channel))
##areawise bouquet count
msrBouqAreaRpt = msrBouquet_bq_all %>% 
  group_by(LCO_CITY,Broadcaster.Name,Bouquet) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day),'Average' = sum(Monthly.Subs.of.the.Channel))

####work on Alacarte####
msrAlacarte = msrAlacarte %>% filter(!(Broadcaster.Name %in% c("ABP News Network Pvt Limited","Free to AIR","Republic TV","DD")))
msrAlacarte_filterred = msrAlacarte %>%
  select(LCO_CITY,Broadcaster.Name,Plan.Name,Channel,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day,
         No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day,Monthly.Subs.of.the.Channel) %>% distinct() %>% na.omit()
msrAlacarte_singlecas = msrBouquet_combo_new %>% filter(X == 'Alacarte') %>% select(LCO_CITY,Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day,
                                                                         No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day,Monthly.Subs.of.the.Channel)
colnames(msrAlacarte_singlecas)[4]<-'Channel'

##join alacarte
msrAlacarte_final = rbind(msrAlacarte_filterred,msrAlacarte_singlecas)

msrAlaRpt = msrAlacarte_final %>% 
  group_by(Broadcaster.Name,Channel) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day),'Average' = sum(Monthly.Subs.of.the.Channel))
###planwise
msrAlaRptPlan = msrAlacarte_final %>% 
  group_by(Broadcaster.Name,Plan.Name,Channel) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day),'Average' = sum(Monthly.Subs.of.the.Channel))
##areawise
msrAlaAreaRpt = msrAlacarte_final %>% 
  group_by(Broadcaster.Name,LCO_CITY,Channel) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day),'Average' = sum(Monthly.Subs.of.the.Channel))

write.xlsx(as.data.frame(msrBouqRpt), file="Output/MSR_Report_all_June24.xlsx", sheetName="Bouquet", row.names=FALSE)
write.xlsx(as.data.frame(msrAlaRpt), file="Output/MSR_Report_all_June24.xlsx", sheetName="Alacarte", append=TRUE, row.names=FALSE)

##planwise
write.xlsx(as.data.frame(msrBouqRptPlan), file="Output/MSR_Report_Planwise_all_June24.xlsx", sheetName="Bouquet", row.names=FALSE)
write.xlsx(as.data.frame(msrAlaRptPlan), file="Output/MSR_Report_Planwise_all_June24.xlsx", sheetName="Alacarte", append=TRUE, row.names=FALSE)

##areawise
write.xlsx(as.data.frame(msrBouqAreaRpt), file="Output/MSR_Report_Areawise_all_June24.xlsx", sheetName="Area_Bouquet", row.names=FALSE)
write.xlsx(as.data.frame(msrAlaAreaRpt), file="Output/MSR_Report_Areawise_all_June24.xlsx", sheetName="Area_Alacarte", append=TRUE, row.names=FALSE)



