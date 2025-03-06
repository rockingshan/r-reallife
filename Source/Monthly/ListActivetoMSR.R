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
bouquet_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=1yk7CDbZghpUUZzWmGbmQyz44baVr688s&export=download")) ###pre 2024 package update link
bouquet_names = bouquet_names %>% add_row(Bouquet = "Bronze Basic")

#singlecode pak
singlepack_7 = read.csv(file.choose())
singlepack_14 = read.csv(file.choose())
singlepack_21 = read.csv(file.choose())
singlepack_28 = read.csv(file.choose())

msrAll7th = active_to_msr_format()
colnames(msrAll7th)[5] <- 'No.of.Subs.On.7th.Day'
msrAll14th = active_to_msr_format()
colnames(msrAll14th)[5] <- 'No.of.Subs.On.14th.Day'
msrAll21th = active_to_msr_format()
colnames(msrAll21th)[5] <- 'No.of.Subs.On.21st.Day'
msrAll28th = active_to_msr_format()
colnames(msrAll28th)[5] <- 'No.of.Subs.On.28th.Day'

msrAllCombo = merge(msrAll7th,msrAll14th, all = T)
msrAllCombo = merge(msrAllCombo,msrAll21th, all = T)
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

###Fix obsolete bouquets####
msrBouquet_bq_all$Bouquet[msrBouquet_bq_all$Bouquet == "WBD Kids SD NT2"] <- "WBD Life SD NT2"
msrBouquet_bq_all$Bouquet[msrBouquet_bq_all$Bouquet == "WBD Kids HD NT2"] <- "WBD Life HD NT2"
msrBouquet_bq_all$Plan.Name[msrBouquet_bq_all$Plan.Name == "WBD Kids SD NT2 @ 14"] <- "WBD Life SD NT2 @ 14"
msrBouquet_bq_all$Plan.Name[msrBouquet_bq_all$Plan.Name == "WBD Kids HD NT2 @ 22"] <- "WBD Life HD NT2 @ 22"
msrBouquet_bq_all$Bouquet[msrBouquet_bq_all$Bouquet == "COLORS WALA BANGLA BUDGET HD NT2"] <- "COLORS WALA BANGLA VALUE HD NT2"
msrBouquet_bq_all$Plan.Name[msrBouquet_bq_all$Plan.Name == "COLORS WALA BANGLA BUDGET HD NT2 @ 32"] <- "COLORS WALA BANGLA VALUE HD NT2 @ 54"
msrBouquet_bq_all$Bouquet[msrBouquet_bq_all$Bouquet == "Colors WalaHindi Budget Plus NT2"] <- "Colors WalaHindi Value Plus NT2"
msrBouquet_bq_all$Plan.Name[msrBouquet_bq_all$Plan.Name == "Colors WalaHindi Budget Plus NT2 @ 25"] <- "Colors WalaHindi Value Plus NT2 @ 34"

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
msrAlacarte_final <- msrAlacarte_final %>%
  filter(!(Channel == "Sun Bangla HD" & Broadcaster.Name == "Eenadu Television Pvt Ltd"))


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

write.xlsx(as.data.frame(msrBouqRpt), file="Output/MSR_Report_all_Feb25.xlsx", sheetName="Bouquet", row.names=FALSE)
write.xlsx(as.data.frame(msrAlaRpt), file="Output/MSR_Report_all_Feb25.xlsx", sheetName="Alacarte", append=TRUE, row.names=FALSE)

##planwise
write.xlsx(as.data.frame(msrBouqRptPlan), file="Output/MSR_Report_Planwise_all_Feb25.xlsx", sheetName="Bouquet", row.names=FALSE)
write.xlsx(as.data.frame(msrAlaRptPlan), file="Output/MSR_Report_Planwise_all_Feb25.xlsx", sheetName="Alacarte", append=TRUE, row.names=FALSE)

##areawise
write.xlsx(as.data.frame(msrBouqAreaRpt), file="Output/MSR_Report_Areawise_all_Feb25.xlsx", sheetName="Area_Bouquet", row.names=FALSE)
write.xlsx(as.data.frame(msrAlaAreaRpt), file="Output/MSR_Report_Areawise_all_Feb25.xlsx", sheetName="Area_Alacarte", append=TRUE, row.names=FALSE)





####For IPTV Reporting####
iptvreport = read.csv(file.choose())
iptvreport = iptvreport %>% group_by(Code) %>%
  summarise(across(contains("Subs"), \(x) sum(x, na.rm = TRUE)))
colnames(iptvreport) <- c("Plan.Name", "Code", "No.of.Subs.On.7th.Day", "No.of.Subs.On.14th.Day.14TH_DAY", "No.of.Subs.On.21st.Day", "No.of.Subs.On.28th.Day.28TH_DAY")
iptvreport = iptvreport %>% group_by(Code) %>%
  summarise(across(contains("Subs"), \(x) sum(x, na.rm = TRUE)))
singlepack_7 = read.csv(file.choose())
singlepack_14 = read.csv(file.choose())
singlepack_21 = read.csv(file.choose())
singlepack_28 = read.csv(file.choose())

iptv_nw_7 = iptvreport %>% select(Code,No.of.Subs.On.7th.Day) %>% unique()
iptv_nw_14 = iptvreport %>% select(Code,No.of.Subs.On.14th.Day.14TH_DAY) %>% unique()
iptv_nw_21 = iptvreport %>% select(Code,No.of.Subs.On.21st.Day) %>% unique()
iptv_nw_28 = iptvreport %>% select(Code,No.of.Subs.On.28th.Day.28TH_DAY) %>% unique()

iptv_nw_7_pk = merge(iptv_nw_7,singlepack_7,all.y = T) %>% unique() %>% unite(combined, c('Code','Bouquet'),sep = "|")
iptv_nw_14_pk = merge(iptv_nw_14,singlepack_14,all.y = F) %>% unique() %>% unite(combined, c('Code','Bouquet'),sep = "|")
iptv_nw_21_pk = merge(iptv_nw_21,singlepack_21,all.y = F) %>% unique() %>% unite(combined, c('Code','Bouquet'),sep = "|")
iptv_nw_28_pk = merge(iptv_nw_28,singlepack_28,all.y = F) %>% unique() %>% unite(combined, c('Code','Bouquet'),sep = "|")
iptv_combo = merge(iptv_nw_7_pk,iptv_nw_14_pk, by.x = "combined", by.y = "combined", all = T)
iptv_combo = merge(iptv_combo,iptv_nw_21_pk, by.x = "combined", by.y = "combined", all = T)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 iptv_combo = merge(iptv_combo, iptv_nw_21_pk,all = T)
iptv_combo = merge(iptv_combo, iptv_nw_28_pk, all = T) %>% separate(combined, into = c("Code","Bouquet"),sep = "\\|")
iptv_combo[is.na(iptv_combo)] <- 0
iptv_combo$No.of.Subs.On.7th.Day = as.numeric(iptv_combo$No.of.Subs.On.7th.Day)
iptv_combo$No.of.Subs.On.14th.Day.14TH_DAY = as.numeric(iptv_combo$No.of.Subs.On.14th.Day.14TH_DAY)
iptv_combo$No.of.Subs.On.21st.Day = as.numeric(iptv_combo$No.of.Subs.On.21st.Day)
iptv_combo$No.of.Subs.On.28th.Day.28TH_DAY = as.numeric(iptv_combo$No.of.Subs.On.28th.Day.28TH_DAY)
iptv_combo = iptv_combo %>% mutate(Monthly.Subs.of.the.Channel = rowMeans(select(iptv_combo, starts_with("No.of"))))

iptv_combo_bouq = iptv_combo %>% filter(X == 'Bouquet') %>% select(Broadcaster.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day.14TH_DAY,
                                                                             No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day.28TH_DAY,Monthly.Subs.of.the.Channel)

bc_name = iptv_combo_bouq %>%
  select(Broadcaster.Name,Bouquet) %>% distinct()%>% na.omit()

iptv_combo_bouq$No.of.Subs.On.7th.Day = as.numeric(iptv_combo_bouq$No.of.Subs.On.7th.Day)
iptv_combo_bouq$No.of.Subs.On.14th.Day.14TH_DAY = as.numeric(iptv_combo_bouq$No.of.Subs.On.14th.Day.14TH_DAY)
iptv_combo_bouq$No.of.Subs.On.21st.Day = as.numeric(iptv_combo_bouq$No.of.Subs.On.21st.Day)
iptv_combo_bouq$No.of.Subs.On.28th.Day.28TH_DAY = as.numeric(iptv_combo_bouq$No.of.Subs.On.28th.Day.28TH_DAY)
iptv_combo_bouq$Monthly.Subs.of.the.Channel = as.numeric(iptv_combo_bouq$Monthly.Subs.of.the.Channel)

active_pivot = iptv_combo_bouq %>% 
  group_by(Bouquet) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day.14TH_DAY),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day.28TH_DAY),'Average' = sum(Monthly.Subs.of.the.Channel))
od_bq_rpt = merge(bc_name,active_pivot)

# active_pivot_bq_pl = iptv_combo_bouq %>% 
#   group_by(Bouquet,Plan.Name) %>%
#   summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day.14TH_DAY),'Active_21st' = sum(No.of.Subs.On.21st.Day),
#             'Active_28th' = sum(No.of.Subs.On.28th.Day.28TH_DAY),'Average' = sum(Monthly.Subs.of.the.Channel))
# od_bq_rpt_pl = merge(bc_name,active_pivot_bq_pl)


iptv_combo_ala = iptv_combo %>% filter(X == 'Alacarte') %>% select(Broadcaster.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day.14TH_DAY,
                                                                         No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day.28TH_DAY,Monthly.Subs.of.the.Channel)
colnames(iptv_combo_ala)[2]<-'Channel'

bc_name = iptv_combo_ala %>%
  select(Broadcaster.Name,Channel) %>% distinct()%>% na.omit()

active_pivot = iptv_combo_ala %>% 
  group_by(Channel) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day.14TH_DAY),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day.28TH_DAY),'Average' = sum(Monthly.Subs.of.the.Channel))
od_al_rpt = merge(bc_name,active_pivot)

# active_pivot_pl = iptv_combo_ala %>% 
#   group_by(Channel,Plan.Name) %>%
#   summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day.14TH_DAY),'Active_21st' = sum(No.of.Subs.On.21st.Day),
#             'Active_28th' = sum(No.of.Subs.On.28th.Day.28TH_DAY),'Average' = sum(Monthly.Subs.of.the.Channel))
# od_al_rpt_pl = merge(bc_name,active_pivot)


##NTO report all
write.xlsx(as.data.frame(od_bq_rpt), file="Output/IPTV_MSR__all_Feb25.xlsx", sheetName="Bouquet", row.names=FALSE)
write.xlsx(as.data.frame(od_al_rpt), file="Output/IPTV_MSR__all_Feb25.xlsx", sheetName="Alacarte", append=TRUE, row.names=FALSE)

# ##NTO report all
# write.xlsx(as.data.frame(od_bq_rpt_pl), file="Output/IPTV_MSR_Planwise_all_Feb25.xlsx", sheetName="Bouquet", row.names=FALSE)
# write.xlsx(as.data.frame(od_al_rpt_pl), file="Output/IPTV_MSR_Planwise_all_Feb25.xlsx", sheetName="Alacarte", append=TRUE, row.names=FALSE)
# 


