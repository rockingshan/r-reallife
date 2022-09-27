library(tidyverse)
library(dplyr)
library(readxl)
library(xlsx)

#this report requires separate Alacarte and bouquet xlsx files saved from excel. direct MQ files not work
#need to create package-service configuration files for 4 weeks

###WORK oDISHA BQ REPORT
bc_odisha = read_xlsx(file.choose(), skip = 3)
names(bc_odisha) = make.names(names(bc_odisha))
bc_odisha = bc_odisha[!grepl("Plan Name", bc_odisha$Plan.Name),]
bc_odisha = bc_odisha[!grepl("Broadcaster Name:*", bc_odisha$Broadcaster.Name),]

bc_odisha_filterred = bc_odisha %>%
  select(Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
         No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel) %>% distinct() %>% na.omit()
bc_name = bc_odisha %>%
  select(Broadcaster.Name,Bouquet) %>% distinct()%>% na.omit()

bc_odisha_filterred$No.of.Subs.On.7th.Day = as.numeric(bc_odisha_filterred$No.of.Subs.On.7th.Day)
bc_odisha_filterred$No.of.Subs.On.14th.Day..14TH_DAY = as.numeric(bc_odisha_filterred$No.of.Subs.On.14th.Day..14TH_DAY)
bc_odisha_filterred$No.of.Subs.On.21st.Day = as.numeric(bc_odisha_filterred$No.of.Subs.On.21st.Day)
bc_odisha_filterred$No.of.Subs.On.28th.Day..28TH_DAY = as.numeric(bc_odisha_filterred$No.of.Subs.On.28th.Day..28TH_DAY)
bc_odisha_filterred$Monthly.Subs.of.the.Channel = as.numeric(bc_odisha_filterred$Monthly.Subs.of.the.Channel)

active_pivot = bc_odisha_filterred %>% 
  group_by(Bouquet) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day..14TH_DAY),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day..28TH_DAY),'Average' = sum(Monthly.Subs.of.the.Channel))
od_bq_rpt = merge(bc_name,active_pivot)

##WORK ODISHA ALACARTE REPORT
al_od = read_xlsx(file.choose(), skip = 3)
names(al_od) = make.names(names(al_od))
al_od = al_od[!grepl("Plan Name", al_od$Plan.Name),]
al_od = al_od[!grepl("Broadcaster Name:*", al_od$Month),]
al_od_filterred = al_od %>%
  select(Broadcaster.Name,Plan.Name,Channel,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
         No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel) %>% distinct() %>% na.omit()
bc_name = al_od %>%
  select(Broadcaster.Name,Channel) %>% distinct()%>% na.omit()
al_od_filterred$No.of.Subs.On.7th.Day = as.numeric(al_od_filterred$No.of.Subs.On.7th.Day)
al_od_filterred$No.of.Subs.On.14th.Day..14TH_DAY = as.numeric(al_od_filterred$No.of.Subs.On.14th.Day..14TH_DAY)
al_od_filterred$No.of.Subs.On.21st.Day = as.numeric(al_od_filterred$No.of.Subs.On.21st.Day)
al_od_filterred$No.of.Subs.On.28th.Day..28TH_DAY = as.numeric(al_od_filterred$No.of.Subs.On.28th.Day..28TH_DAY)
al_od_filterred$Monthly.Subs.of.the.Channel = as.numeric(al_od_filterred$Monthly.Subs.of.the.Channel)
active_pivot = al_od_filterred %>% 
  group_by(Channel) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day..14TH_DAY),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day..28TH_DAY),'Average' = sum(Monthly.Subs.of.the.Channel))
od_al_rpt = merge(bc_name,active_pivot)


####WORK ON WB BOUQUWT
bq_wb = read_xlsx(file.choose(), skip = 3)
names(bq_wb) = make.names(names(bq_wb))
bq_wb = bq_wb[!grepl("Plan Name", bq_wb$Plan.Name),]
bq_wb = bq_wb[!grepl("Broadcaster Name:*", bq_wb$Broadcaster.Name),]
bq_wb = bq_wb[!grepl("Plan Name", bq_wb$Plan.Name),]
bq_wb = bq_wb[!grepl("Broadcaster Name:*", bq_wb$Broadcaster.Name),]
bq_wb_old_pack = bq_wb[!grepl("_", bq_wb$Plan.Name),] %>% select(Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
                                                                 No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel)
bq_wb_new_pack = bq_wb[grepl("_", bq_wb$Plan.Name),]
bqwbnew_7 = bq_wb_new_pack %>% select(Plan.Name,No.of.Subs.On.7th.Day)
bqwbnew_14 = bq_wb_new_pack %>% select(Plan.Name,No.of.Subs.On.14th.Day..14TH_DAY)
bqwbnew_21 = bq_wb_new_pack %>% select(Plan.Name,No.of.Subs.On.21st.Day)
bqwbnew_28 = bq_wb_new_pack %>% select(Plan.Name,No.of.Subs.On.28th.Day..28TH_DAY)
bqwbnew_avg = bq_wb_new_pack %>% select(Plan.Name,Monthly.Subs.of.the.Channel)
pack_7 = read.csv(file.choose())
pack_14 = read.csv(file.choose())
pack_21 = read.csv(file.choose())
pack_28 = read.csv(file.choose())
bqwbnew_7_pk = merge(bqwbnew_7,pack_7,all = T) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")
bqwbnew_14_pk = merge(bqwbnew_14,pack_14,all = T) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")
bqwbnew_21_pk = merge(bqwbnew_21,pack_21,all = T) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")
bqwbnew_28_pk = merge(bqwbnew_28,pack_28,all = T) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")
bqwbnew_combo = merge(bqwbnew_7_pk,bqwbnew_14_pk,all = T)
bqwbnew_combo = merge(bqwbnew_combo, bqwbnew_21_pk, all = T)
bqwbnew_combo = merge(bqwbnew_combo, bqwbnew_28_pk, all = T) %>% separate(combined, into = c("Plan.Name","Bouquet"),sep = "\\|")
bqwbnew_combo[is.na(bqwbnew_combo)] <- 0
bqwbnew_combo$No.of.Subs.On.7th.Day = as.numeric(bqwbnew_combo$No.of.Subs.On.7th.Day)
bqwbnew_combo$No.of.Subs.On.14th.Day..14TH_DAY = as.numeric(bqwbnew_combo$No.of.Subs.On.14th.Day..14TH_DAY)
bqwbnew_combo$No.of.Subs.On.21st.Day = as.numeric(bqwbnew_combo$No.of.Subs.On.21st.Day)
bqwbnew_combo$No.of.Subs.On.28th.Day..28TH_DAY = as.numeric(bqwbnew_combo$No.of.Subs.On.28th.Day..28TH_DAY)
bqwbnew_combo = bqwbnew_combo %>% mutate(Monthly.Subs.of.the.Channel = rowMeans(select(bqwbnew_combo, starts_with("No.of"))))
bqwbnew_combo_bouq = bqwbnew_combo %>% filter(X == 'Bouquet') %>% select(Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
                                                                         No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel)
wb_bouquet_final = rbind(bq_wb_old_pack,bqwbnew_combo_bouq)
wb_bouquet_final$No.of.Subs.On.7th.Day = as.numeric(wb_bouquet_final$No.of.Subs.On.7th.Day)
wb_bouquet_final$No.of.Subs.On.14th.Day..14TH_DAY = as.numeric(wb_bouquet_final$No.of.Subs.On.14th.Day..14TH_DAY)
wb_bouquet_final$No.of.Subs.On.21st.Day = as.numeric(wb_bouquet_final$No.of.Subs.On.21st.Day)
wb_bouquet_final$No.of.Subs.On.28th.Day..28TH_DAY = as.numeric(wb_bouquet_final$No.of.Subs.On.28th.Day..28TH_DAY)
wb_bouquet_final$Monthly.Subs.of.the.Channel = as.numeric(wb_bouquet_final$Monthly.Subs.of.the.Channel)
wbbqt_active_pivot = wb_bouquet_final %>% 
  group_by(Bouquet,Broadcaster.Name) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day..14TH_DAY),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day..28TH_DAY),'Average' = sum(Monthly.Subs.of.the.Channel))

########WORK ON WB ALACARTE
al_wb = read_xlsx(file.choose(), skip = 3)
names(al_wb) = make.names(names(al_wb))
al_wb = al_wb[!grepl("Plan Name", al_wb$Plan.Name),]
al_wb = al_wb[!grepl("Broadcaster Name:*", al_wb$Month),]
al_wb_filterred = al_wb %>%
  select(Broadcaster.Name,Plan.Name,Channel,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
         No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel) %>% distinct() %>% na.omit()
al_wb_filterred$No.of.Subs.On.7th.Day = as.numeric(al_wb_filterred$No.of.Subs.On.7th.Day)
al_wb_filterred$No.of.Subs.On.14th.Day..14TH_DAY = as.numeric(al_wb_filterred$No.of.Subs.On.14th.Day..14TH_DAY)
al_wb_filterred$No.of.Subs.On.21st.Day = as.numeric(al_wb_filterred$No.of.Subs.On.21st.Day)
al_wb_filterred$No.of.Subs.On.28th.Day..28TH_DAY = as.numeric(al_wb_filterred$No.of.Subs.On.28th.Day..28TH_DAY)
al_wb_filterred$Monthly.Subs.of.the.Channel = as.numeric(al_wb_filterred$Monthly.Subs.of.the.Channel)
wb_new_ala = bqwbnew_combo %>% filter(X == 'Alacarte') %>% select(Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
                                                                         No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel)
colnames(wb_new_ala)[3]<-'Channel'
wb_new_ala_final = rbind(al_wb_filterred,wb_new_ala)

active_pivot_ala_wb = wb_new_ala_final %>% 
  group_by(Channel,Broadcaster.Name) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day..14TH_DAY),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day..28TH_DAY),'Average' = sum(Monthly.Subs.of.the.Channel))

write.xlsx(as.data.frame(od_bq_rpt), file="Output/MSR_Report_all_Sep22.xlsx", sheetName="Odisha_Bouquet", row.names=FALSE)
write.xlsx(as.data.frame(od_al_rpt), file="Output/MSR_Report_all_Sep22.xlsx", sheetName="Odisha_Alacarte", append=TRUE, row.names=FALSE)
write.xlsx(as.data.frame(wbbqt_active_pivot), file="Output/MSR_Report_all_Sep22.xlsx", sheetName="WB_Bouquet", append=TRUE,row.names=FALSE)
write.xlsx(as.data.frame(active_pivot_ala_wb), file="Output/MSR_Report_all_Sep22.xlsx", sheetName="WB_Alacarte", append=TRUE, row.names=FALSE)