library(tidyverse)
library(dplyr)
library(readxl)
library(xlsx)
library(stringr)

#this report requires separate Alacarte and bouquet xlsx files saved from excel. direct MQ files not work
#need to create package-service configuration files for 4 weeks
###Single cas code pack names from gdrive
bouquet_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=1yk7CDbZghpUUZzWmGbmQyz44baVr688s&export=download"))
###WORK oDISHA BQ REPORT ####
bc_odisha = read_xlsx(file.choose(), skip = 3)
names(bc_odisha) = make.names(names(bc_odisha))
bc_odisha = bc_odisha[!grepl("Plan Name", bc_odisha$Plan.Name),]
bc_odisha = bc_odisha[!grepl("Broadcaster Name:*", bc_odisha$Broadcaster.Name),]
##calculate new count
bc_odisha_nw = bc_odisha %>% filter(Bouquet %in% bouquet_names$Bouquet) %>% select(Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
                                                                                   No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel)
bc_odisha_nw_7 = bc_odisha_nw %>% select(Plan.Name,No.of.Subs.On.7th.Day) %>% unique()
bc_odisha_nw_14 = bc_odisha_nw %>% select(Plan.Name,No.of.Subs.On.14th.Day..14TH_DAY) %>% unique()
bc_odisha_nw_21 = bc_odisha_nw %>% select(Plan.Name,No.of.Subs.On.21st.Day) %>% unique()
bc_odisha_nw_28 = bc_odisha_nw %>% select(Plan.Name,No.of.Subs.On.28th.Day..28TH_DAY) %>% unique()
bc_odisha_nw_avg = bc_odisha_nw %>% select(Plan.Name,Monthly.Subs.of.the.Channel) %>% unique()
#singlecode pak
singlepack_7 = read.csv(file.choose())
singlepack_14 = read.csv(file.choose())
singlepack_21 = read.csv(file.choose())
singlepack_28 = read.csv(file.choose())
bc_odisha_nw_7_pk = merge(bc_odisha_nw_7,singlepack_7,all.y = F) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")
bc_odisha_nw_14_pk = merge(bc_odisha_nw_14,singlepack_14,all.y = F) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")
bc_odisha_nw_21_pk = merge(bc_odisha_nw_21,singlepack_21,all.y = F) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")
bc_odisha_nw_28_pk = merge(bc_odisha_nw_28,singlepack_28,all.y = F) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")
bc_odisha_combo = merge(bc_odisha_nw_7_pk,bc_odisha_nw_14_pk, all = T)
bc_odisha_combo = merge(bc_odisha_combo, bc_odisha_nw_21_pk,all = T)
bc_odisha_combo = merge(bc_odisha_combo, bc_odisha_nw_28_pk, all = T) %>% separate(combined, into = c("Plan.Name","Bouquet"),sep = "\\|")
bc_odisha_combo[is.na(bc_odisha_combo)] <- 0
bc_odisha_combo$No.of.Subs.On.7th.Day = as.numeric(bc_odisha_combo$No.of.Subs.On.7th.Day)
bc_odisha_combo$No.of.Subs.On.14th.Day..14TH_DAY = as.numeric(bc_odisha_combo$No.of.Subs.On.14th.Day..14TH_DAY)
bc_odisha_combo$No.of.Subs.On.21st.Day = as.numeric(bc_odisha_combo$No.of.Subs.On.21st.Day)
bc_odisha_combo$No.of.Subs.On.28th.Day..28TH_DAY = as.numeric(bc_odisha_combo$No.of.Subs.On.28th.Day..28TH_DAY)
bc_odisha_combo = bc_odisha_combo %>% mutate(Monthly.Subs.of.the.Channel = rowMeans(select(bc_odisha_combo, starts_with("No.of"))))

bc_odisha_combo_bouq = bc_odisha_combo %>% filter(X == 'Bouquet') %>% select(Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
                                                                         No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel)

##calculate old process
bc_odisha_old = bc_odisha %>% filter(!(Bouquet %in% bouquet_names$Bouquet))
bc_odisha_filterred = bc_odisha_old %>%
  select(Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
         No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel) %>% distinct() %>% na.omit()
##add single cas code pack
bc_odisha_bq_all = rbind(bc_odisha_filterred,bc_odisha_combo_bouq)

bc_name = bc_odisha_bq_all %>%
  select(Broadcaster.Name,Bouquet) %>% distinct()%>% na.omit()

bc_odisha_bq_all$No.of.Subs.On.7th.Day = as.numeric(bc_odisha_bq_all$No.of.Subs.On.7th.Day)
bc_odisha_bq_all$No.of.Subs.On.14th.Day..14TH_DAY = as.numeric(bc_odisha_bq_all$No.of.Subs.On.14th.Day..14TH_DAY)
bc_odisha_bq_all$No.of.Subs.On.21st.Day = as.numeric(bc_odisha_bq_all$No.of.Subs.On.21st.Day)
bc_odisha_bq_all$No.of.Subs.On.28th.Day..28TH_DAY = as.numeric(bc_odisha_bq_all$No.of.Subs.On.28th.Day..28TH_DAY)
bc_odisha_bq_all$Monthly.Subs.of.the.Channel = as.numeric(bc_odisha_bq_all$Monthly.Subs.of.the.Channel)

active_pivot = bc_odisha_bq_all %>% 
  group_by(Bouquet) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day..14TH_DAY),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day..28TH_DAY),'Average' = sum(Monthly.Subs.of.the.Channel))
od_bq_rpt = merge(bc_name,active_pivot)

##WORK ODISHA ALACARTE REPORT ####
al_od = read_xlsx(file.choose(), skip = 3)
names(al_od) = make.names(names(al_od))
al_od = al_od[!grepl("Plan Name", al_od$Plan.Name),]
al_od = al_od[!grepl("Broadcaster Name:*", al_od$Month),]
al_od_filterred = al_od %>%
  select(Broadcaster.Name,Plan.Name,Channel,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
         No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel) %>% distinct() %>% na.omit()

al_od_filterred$No.of.Subs.On.7th.Day = as.numeric(al_od_filterred$No.of.Subs.On.7th.Day)
al_od_filterred$No.of.Subs.On.14th.Day..14TH_DAY = as.numeric(al_od_filterred$No.of.Subs.On.14th.Day..14TH_DAY)
al_od_filterred$No.of.Subs.On.21st.Day = as.numeric(al_od_filterred$No.of.Subs.On.21st.Day)
al_od_filterred$No.of.Subs.On.28th.Day..28TH_DAY = as.numeric(al_od_filterred$No.of.Subs.On.28th.Day..28TH_DAY)
al_od_filterred$Monthly.Subs.of.the.Channel = as.numeric(al_od_filterred$Monthly.Subs.of.the.Channel)
##add new alacarte from single cas code
al_od_singlecas = bc_odisha_combo %>% filter(X == 'Alacarte') %>% select(Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
                                                                        No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel)
colnames(al_od_singlecas)[3]<-'Channel'
##join alacarte
al_od_final = rbind(al_od_filterred,al_od_singlecas)
bc_name = al_od_final %>%
  select(Broadcaster.Name,Channel) %>% distinct()%>% na.omit()

active_pivot = al_od_final %>% 
  group_by(Channel) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day..14TH_DAY),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day..28TH_DAY),'Average' = sum(Monthly.Subs.of.the.Channel))
od_al_rpt = merge(bc_name,active_pivot)


####WORK ON WB BOUQUET ####
bq_wb = read_xlsx(file.choose(), skip = 3)
names(bq_wb) = make.names(names(bq_wb))
bq_wb = bq_wb[!grepl("Plan Name", bq_wb$Plan.Name),]
bq_wb = bq_wb[!grepl("Broadcaster Name:*", bq_wb$Broadcaster.Name),]
bq_wb = bq_wb[!grepl("Plan Name", bq_wb$Plan.Name),]
bq_wb = bq_wb[!grepl("Broadcaster Name:*", bq_wb$Broadcaster.Name),]
##different single cas code pack and calculate
bq_wb_nw_sncd = bq_wb %>% filter(Bouquet %in% bouquet_names$Bouquet) %>% select(Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
                                                                                   No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel)
bq_wb_nw_sncd_7 = bq_wb_nw_sncd %>% select(Plan.Name,No.of.Subs.On.7th.Day) %>% unique()
bq_wb_nw_sncd_14 = bq_wb_nw_sncd %>% select(Plan.Name,No.of.Subs.On.14th.Day..14TH_DAY) %>% unique()
bq_wb_nw_sncd_21 = bq_wb_nw_sncd %>% select(Plan.Name,No.of.Subs.On.21st.Day) %>% unique()
bq_wb_nw_sncd_28 = bq_wb_nw_sncd %>% select(Plan.Name,No.of.Subs.On.28th.Day..28TH_DAY) %>% unique()
bq_wb_nw_sncd_avg = bq_wb_nw_sncd %>% select(Plan.Name,Monthly.Subs.of.the.Channel) %>% unique()
#singlecode pak
bq_wb_nw_sncd_7_pk = merge(bq_wb_nw_sncd_7,singlepack_7,all.y = F) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")
bq_wb_nw_sncd_14_pk = merge(bq_wb_nw_sncd_14,singlepack_14,all.y = F) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")
bq_wb_nw_sncd_21_pk = merge(bq_wb_nw_sncd_21,singlepack_21,all.y = F) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")
bq_wb_nw_sncd_28_pk = merge(bq_wb_nw_sncd_28,singlepack_28,all.y = F) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")

bq_wb_nw_sncd_combo = merge(bq_wb_nw_sncd_7_pk,bq_wb_nw_sncd_14_pk, all = T)
bq_wb_nw_sncd_combo = merge(bq_wb_nw_sncd_combo, bq_wb_nw_sncd_21_pk,all = T)
bq_wb_nw_sncd_combo = merge(bq_wb_nw_sncd_combo, bq_wb_nw_sncd_28_pk, all = T) %>% separate(combined, into = c("Plan.Name","Bouquet"),sep = "\\|")
bq_wb_nw_sncd_combo[is.na(bq_wb_nw_sncd_combo)] <- 0

bq_wb_nw_sncd_combo$No.of.Subs.On.7th.Day = as.numeric(bq_wb_nw_sncd_combo$No.of.Subs.On.7th.Day)
bq_wb_nw_sncd_combo$No.of.Subs.On.14th.Day..14TH_DAY = as.numeric(bq_wb_nw_sncd_combo$No.of.Subs.On.14th.Day..14TH_DAY)
bq_wb_nw_sncd_combo$No.of.Subs.On.21st.Day = as.numeric(bq_wb_nw_sncd_combo$No.of.Subs.On.21st.Day)
bq_wb_nw_sncd_combo$No.of.Subs.On.28th.Day..28TH_DAY = as.numeric(bq_wb_nw_sncd_combo$No.of.Subs.On.28th.Day..28TH_DAY)
bq_wb_nw_sncd_combo = bq_wb_nw_sncd_combo %>% mutate(Monthly.Subs.of.the.Channel = rowMeans(select(bq_wb_nw_sncd_combo, starts_with("No.of"))))

bq_wb_nw_sncd_combo_bouq = bq_wb_nw_sncd_combo %>% filter(X == 'Bouquet') %>% select(Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
                                                                             No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel)

####old pacakges working
bc_wb_old = bq_wb %>% filter(!(Bouquet %in% bouquet_names$Bouquet))

bq_wb_old_pack = bc_wb_old[!grepl("_", bc_wb_old$Plan.Name),] %>% select(Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
                                                                 No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel)
bq_wb_new_pack = bc_wb_old[grepl("_", bc_wb_old$Plan.Name),]
bqwbnew_7 = bq_wb_new_pack %>% select(Plan.Name,No.of.Subs.On.7th.Day) %>% unique()
bqwbnew_14 = bq_wb_new_pack %>% select(Plan.Name,No.of.Subs.On.14th.Day..14TH_DAY) %>% unique()
bqwbnew_21 = bq_wb_new_pack %>% select(Plan.Name,No.of.Subs.On.21st.Day) %>% unique()
bqwbnew_28 = bq_wb_new_pack %>% select(Plan.Name,No.of.Subs.On.28th.Day..28TH_DAY) %>% unique()
bqwbnew_avg = bq_wb_new_pack %>% select(Plan.Name,Monthly.Subs.of.the.Channel) %>% unique()
pack_7 = read.csv(file.choose())
pack_14 = read.csv(file.choose())
pack_21 = read.csv(file.choose())
pack_28 = read.csv(file.choose())
bqwbnew_7_pk = merge(bqwbnew_7,pack_7,all = T) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")
bqwbnew_14_pk = merge(bqwbnew_14,pack_14,all = T) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")
bqwbnew_21_pk = merge(bqwbnew_21,pack_21,all = T) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")
bqwbnew_28_pk = merge(bqwbnew_28,pack_28,all = T) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")
bqwbnew_combo = merge(bqwbnew_7_pk,bqwbnew_14_pk, all = T)
bqwbnew_combo = merge(bqwbnew_combo, bqwbnew_21_pk, all = T)
bqwbnew_combo = merge(bqwbnew_combo, bqwbnew_28_pk, all = T) %>% separate(combined, into = c("Plan.Name","Bouquet"),sep = "\\|")
bqwbnew_combo[is.na(bqwbnew_combo)] <- 0
bqwbnew_combo$No.of.Subs.On.7th.Day = as.numeric(bqwbnew_combo$No.of.Subs.On.7th.Day)
bqwbnew_combo$No.of.Subs.On.14th.Day..14TH_DAY = as.numeric(bqwbnew_combo$No.of.Subs.On.14th.Day..14TH_DAY)
bqwbnew_combo$No.of.Subs.On.21st.Day = as.numeric(bqwbnew_combo$No.of.Subs.On.21st.Day)
bqwbnew_combo$No.of.Subs.On.28th.Day..28TH_DAY = as.numeric(bqwbnew_combo$No.of.Subs.On.28th.Day..28TH_DAY)
bqwbnew_combo = bqwbnew_combo %>% mutate(Monthly.Subs.of.the.Channel = rowMeans(select(bqwbnew_combo, starts_with("No.of"))))
#bqwbnew_combo = read.csv(file.choose())
bqwbnew_combo_bouq = bqwbnew_combo %>% filter(X == 'Bouquet') %>% select(Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
                                                                         No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel)
wb_bouquet_final = rbind(bq_wb_old_pack,bqwbnew_combo_bouq,bq_wb_nw_sncd_combo_bouq)
wb_bouquet_final$No.of.Subs.On.7th.Day = as.numeric(wb_bouquet_final$No.of.Subs.On.7th.Day)
wb_bouquet_final$No.of.Subs.On.14th.Day..14TH_DAY = as.numeric(wb_bouquet_final$No.of.Subs.On.14th.Day..14TH_DAY)
wb_bouquet_final$No.of.Subs.On.21st.Day = as.numeric(wb_bouquet_final$No.of.Subs.On.21st.Day)
wb_bouquet_final$No.of.Subs.On.28th.Day..28TH_DAY = as.numeric(wb_bouquet_final$No.of.Subs.On.28th.Day..28TH_DAY)
wb_bouquet_final$Monthly.Subs.of.the.Channel = as.numeric(wb_bouquet_final$Monthly.Subs.of.the.Channel)
wbbqt_active_pivot = wb_bouquet_final %>% 
  group_by(Bouquet,Broadcaster.Name) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day..14TH_DAY),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day..28TH_DAY),'Average' = sum(Monthly.Subs.of.the.Channel))

######## WORK ON WB ALACARTE #####
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
##add from single cascode
al_wb_singlecas = bq_wb_nw_sncd_combo %>% filter(X == 'Alacarte') %>% select(Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
                                                                         No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel)
colnames(al_wb_singlecas)[3]<-'Channel'

wb_new_ala_final = rbind(al_wb_filterred,wb_new_ala,al_wb_singlecas)



active_pivot_ala_wb = wb_new_ala_final %>% 
  group_by(Channel,Broadcaster.Name) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day..14TH_DAY),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day..28TH_DAY),'Average' = sum(Monthly.Subs.of.the.Channel))
#### print in excel ####

write.xlsx(as.data.frame(od_bq_rpt), file="Output/MSR_Report_all_Apr23.xlsx", sheetName="Odisha_Bouquet", row.names=FALSE)
write.xlsx(as.data.frame(od_al_rpt), file="Output/MSR_Report_all_Apr23.xlsx", sheetName="Odisha_Alacarte", append=TRUE, row.names=FALSE)
write.xlsx(as.data.frame(wbbqt_active_pivot), file="Output/MSR_Report_all_Apr23.xlsx", sheetName="WB_Bouquet", append=TRUE,row.names=FALSE)
write.xlsx(as.data.frame(active_pivot_ala_wb), file="Output/MSR_Report_all_Apr23.xlsx", sheetName="WB_Alacarte", append=TRUE, row.names=FALSE)

###Required by STar ####
od_bq_rpt = bc_odisha_bq_all %>% filter(Broadcaster.Name == "Star India Pvt. Ltd.") %>%
  group_by(Bouquet,Plan.Name) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day..14TH_DAY),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day..28TH_DAY),'Average' = sum(Monthly.Subs.of.the.Channel))


od_al_rpt = al_od_final %>% filter(Broadcaster.Name == "Star India Pvt. Ltd.") %>%
  group_by(Channel,Plan.Name) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day..14TH_DAY),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day..28TH_DAY),'Average' = sum(Monthly.Subs.of.the.Channel))


wbbqt_active_pivot = wb_bouquet_final %>% filter(Broadcaster.Name == "Star India Pvt. Ltd.") %>%
  group_by(Bouquet,Plan.Name) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day..14TH_DAY),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day..28TH_DAY),'Average' = sum(Monthly.Subs.of.the.Channel))

active_pivot_ala_wb = wb_new_ala_final %>% filter(Broadcaster.Name == "Star India Pvt. Ltd.") %>%
  group_by(Channel,Plan.Name) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day..14TH_DAY),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day..28TH_DAY),'Average' = sum(Monthly.Subs.of.the.Channel))



##Data Required by Zee ####
list_bouquet_dated = read.csv(file.choose(new = F)) #import MQ data bouquet
list_alacarte = read.csv(file.choose(new = F)) #import MQ alacarte details
inv = read.csv(file.choose(new = F))
plan_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=17GoiwT4nWCn0J_7HJF0ZyL5Y0-JPNwOJ&export=download"))
discon = read.csv(file.choose(new = F))
###
discon_fl = discon %>% filter(CONTRACT_NUMBER_6 > 11070860) %>% select(Customer.Number,Set.Top.Box.Number) %>% filter(!(Set.Top.Box.Number == "No STB")) %>% unique()
discon_fl$Set.Top.Box.Number = gsub("'","",discon_fl$Set.Top.Box.Number)
###merges with inventory file and return set top box type
discon_type = merge(discon_fl,(inv %>% select(SERIAL_NUMBER,ITEM_DESCR) %>% unique()),by.x = "Set.Top.Box.Number",by.y = "SERIAL_NUMBER",all.x = T)
### 1 - disconnected by box type
discon_type_hd = discon_type %>% filter(str_detect(ITEM_DESCR, "HD"))
discon_type_sd = discon_type %>% filter(!(str_detect(ITEM_DESCR, "HD")))
####
bq_acc = list_bouquet_dated %>% select(Customer.Number,Broadcaster.Name) %>% unique()
al_acc = list_alacarte %>% select(Customer.Number,Broadcaster.Name) %>% unique()
al_bc_acc = rbind(bq_acc,al_acc) %>% unique()
pay_bvc = c("Star India Pvt. Ltd.","Zee Entertainment  Enterprises Limited","Discovery Communications India","Sony Pictures Networks India Pvt. Ltd.","IndiaCast Media Distribution Pvt. Ltd.","New Delhi Television Ltd","WarnerMedia India Pvt. Ltd","TV Today Network Ltd.","SUN Distribution Services Pvt. Ltd.","Zee Media Corporation Ltd","Raj Tv","Bennet Coleman & Company Ltd","IN 10 MEDIA PVT LTD","Eenadu Television Pvt Ltd","Celebrities Management Pvt Ltd","Odisha Television  Ltd","SIDHARTH BROADCASTING PRIVATE LIMITED","BBC Global News India Pvt. Ltd","FOOD FOOD","Lex Sportel Vision Pvt. Ltd.","Tarang Broadcasting Company Limited")
fta_list = al_bc_acc %>% filter(!(Broadcaster.Name %in% pay_bvc)) %>% select(Customer.Number) %>% unique()
pay_list = al_bc_acc %>% filter(Broadcaster.Name %in% pay_bvc)
###number 3 - Only FTA  subscriber
fta_pay = merge(fta_list,pay_list, by = "Customer.Number", all = T) %>% filter(is.na(Broadcaster.Name))
####
bq_acc = list_bouquet_dated %>% select(Customer.Number,Set.Top.Box,Broadcaster.Name) %>% filter(Broadcaster.Name == "Zee Entertainment  Enterprises Limited") %>% unique()
al_acc = list_alacarte %>% select(Customer.Number,Set.Top.Box,Broadcaster.Name) %>% filter(Broadcaster.Name == "Zee Entertainment  Enterprises Limited") %>% unique()
al_bc_acc = rbind(bq_acc,al_acc) %>% unique()
al_bc_acc$Set.Top.Box <- gsub("'","",al_bc_acc$Set.Top.Box)
al_bc_box = merge(al_bc_acc,(inv %>% select(SERIAL_NUMBER,ITEM_DESCR) %>% unique()),by.x = "Set.Top.Box",by.y = "SERIAL_NUMBER",all.x = T)
######### 4 -  subscribing to any of ZEEL Channels (Split by SD & HD)
al_bc_hd = al_bc_box %>% filter(str_detect(ITEM_DESCR, "HD"))
al_bc_sd = al_bc_box %>% filter(!(str_detect(ITEM_DESCR, "HD")))
####
plan = list_bouquet_dated %>% select(Customer.Number,Plan.Name) %>% unique()
oplan_dpo = filter(plan, Plan.Name %in% plan_names$Plan.Name)

oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Diamond_Dig_380 (Promotional)"] <- "DIAMOND DIGITAL @ 380"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "DIAMOND DIGITAL @ 380 (Promotional)"] <- "DIAMOND DIGITAL @ 380"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Gold_353 (Promotional)"] <- "GOLD DIGITAL @ 353"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "GOLD DIGITAL @ 353 (Promotional)"] <- "GOLD DIGITAL @ 353"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Hd_Dha_300(Promotional)"] <- "HD DHAMAKA @ 300"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "HD DHAMAKA @ 300(Promotional)"] <- "HD DHAMAKA @ 300"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Mb_Hd_Beng_455 (Promotional)"] <- "MB BANGLA HD 1 @ 455"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Mb_Hd_Beng_550 (Promotional)"] <- "MB BANGLA HD 2 @ 550"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "MB BANGLA HD 1 @ 455 (Promotional)"] <- "MB BANGLA HD 1 @ 455"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "MB BANGLA HD 2 @ 550 (Promotional)"] <- "MB BANGLA HD 2 @ 550"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Silver_Pow_276 (Promotional)"] <- "Silver Digital Power @ 276"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Silver Digital Power @ 276 (Promotional)"] <- "Silver Digital Power @ 276"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Silver Digital Special @ 276"] <- "Silver Digital Power @ 276"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Meghbela_Bon_330 (Promotional)"] <- "Meghbela Bonanza @ 330"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Meghbela Bonanza @ 330 (Promotional)"] <- "Meghbela Bonanza @ 330"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Meghbela Bonanza Special @ 330"] <- "Meghbela Bonanza @ 330"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Meghbela_Pac_185(Promotional)"] <- "Meghbela Starter Pack @ 185"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Meghbela Starter Pack @ 185 (Promotional)"] <- "Meghbela Starter Pack @ 185"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Meghbela Basic Pack @ 155 (Promo)"] <- "Meghbela Basic Pack @ 155"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Platinum_Dig_450 (Promotional)"] <- "PLATINUM DIGITAL @ 450"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "PLATINUM DIGITAL @ 450 (Promotional)"] <- "PLATINUM DIGITAL @ 450"

oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Diamond_Dig_380"] <- "DIAMOND DIGITAL @ 380"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Gold_Dig_353"] <- "GOLD DIGITAL @ 353"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Hd_Dha_300"] <- "HD DHAMAKA @ 300"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Mb_Hd_Beng_455"] <- "MB BANGLA HD 1 @ 455"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Mb_Hd_Beng_550"] <- "MB BANGLA HD 2 @ 550"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Silver_Pow_276"] <- "Silver Digital Power @ 276"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Meghbela_Bon_330"] <- "Meghbela Bonanza @ 330"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Meghbela_Pac_185"] <- "Meghbela Starter Pack @ 185"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Platinum_Dig_450"] <- "PLATINUM DIGITAL @ 450"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Meghbela_Sta_165"] <- "Meghbela Bengali Starter @165"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Mb_Hd_Hin_455"] <- "MB HINDI HD 1 @ 455"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Mb_Hd_Hin_550"] <- "MB HINDI HD 2 @ 550"

oplan_pivot = oplan_dpo %>% group_by(Plan.Name) %>% summarize(SubsCount = n())
#####5 report
write.csv(oplan_pivot,"Output/5_DPO_plan_count_Apr23.csv",row.names = F)


