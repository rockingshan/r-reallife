library(tidyverse)
library(dplyr)
library(readxl)
library(xlsx)
library(stringr)

#this report requires separate Alacarte and bouquet xlsx files saved from excel. direct MQ files not work
#need to create package-service configuration files for 4 weeks
###Single cas code pack names from gdrive
bouquet_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=1yk7CDbZghpUUZzWmGbmQyz44baVr688s&export=download"))
lco_city = read.csv(file.choose())
###WORK oDISHA BQ REPORT ####
bc_odisha = read_xlsx(file.choose(), skip = 1)
names(bc_odisha) = make.names(names(bc_odisha))
bc_odisha = bc_odisha[!grepl("DPO Promotional Bundle", bc_odisha$Plan.Name),]
##calculate new count
bc_odisha_nw = bc_odisha %>% filter(Bouquet %in% bouquet_names$Bouquet) %>% select(Entity.Code,Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
                                                                                   No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel)
bc_odisha_nw_7 = bc_odisha_nw %>% select(Entity.Code,Plan.Name,No.of.Subs.On.7th.Day) %>% unique()
bc_odisha_nw_14 = bc_odisha_nw %>% select(Entity.Code,Plan.Name,No.of.Subs.On.14th.Day..14TH_DAY) %>% unique()
bc_odisha_nw_21 = bc_odisha_nw %>% select(Entity.Code,Plan.Name,No.of.Subs.On.21st.Day) %>% unique()
bc_odisha_nw_28 = bc_odisha_nw %>% select(Entity.Code,Plan.Name,No.of.Subs.On.28th.Day..28TH_DAY) %>% unique()
bc_odisha_nw_avg = bc_odisha_nw %>% select(Entity.Code,Plan.Name,Monthly.Subs.of.the.Channel) %>% unique()
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

bc_odisha_combo_bouq = bc_odisha_combo %>% filter(X == 'Bouquet') %>% select(Entity.Code,Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
                                                                             No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel)

##calculate old process
bc_odisha_old = bc_odisha %>% filter(!(Bouquet %in% bouquet_names$Bouquet))
bc_odisha_filterred = bc_odisha_old %>%
  select(Entity.Code,Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
         No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel) %>% distinct() %>% na.omit()
##add single cas code pack
bc_odisha_bq_all = rbind(bc_odisha_filterred,bc_odisha_combo_bouq)
bc_odisha_bq_all = merge(bc_odisha_bq_all,lco_city)

bc_name = bc_odisha_bq_all %>%
  select(Broadcaster.Name,Bouquet) %>% distinct()%>% na.omit()

bc_odisha_bq_all$No.of.Subs.On.7th.Day = as.numeric(bc_odisha_bq_all$No.of.Subs.On.7th.Day)
bc_odisha_bq_all$No.of.Subs.On.14th.Day..14TH_DAY = as.numeric(bc_odisha_bq_all$No.of.Subs.On.14th.Day..14TH_DAY)
bc_odisha_bq_all$No.of.Subs.On.21st.Day = as.numeric(bc_odisha_bq_all$No.of.Subs.On.21st.Day)
bc_odisha_bq_all$No.of.Subs.On.28th.Day..28TH_DAY = as.numeric(bc_odisha_bq_all$No.of.Subs.On.28th.Day..28TH_DAY)
bc_odisha_bq_all$Monthly.Subs.of.the.Channel = as.numeric(bc_odisha_bq_all$Monthly.Subs.of.the.Channel)

active_pivot = bc_odisha_bq_all %>% 
  group_by(City,Bouquet) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day..14TH_DAY),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day..28TH_DAY),'Average' = sum(Monthly.Subs.of.the.Channel))
od_bq_rpt = merge(bc_name,active_pivot)


##WORK ODISHA ALACARTE REPORT ####
al_od = read_xlsx(file.choose(), skip = 1)
names(al_od) = make.names(names(al_od))
al_od = al_od[!grepl("DPO Promotional Bundle", al_od$Plan.Name),]
al_od_filterred = al_od %>%
  select(Entity.Code,Broadcaster.Name,Plan.Name,Channel,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
         No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel) %>% distinct() %>% na.omit()

al_od_filterred$No.of.Subs.On.7th.Day = as.numeric(al_od_filterred$No.of.Subs.On.7th.Day)
al_od_filterred$No.of.Subs.On.14th.Day..14TH_DAY = as.numeric(al_od_filterred$No.of.Subs.On.14th.Day..14TH_DAY)
al_od_filterred$No.of.Subs.On.21st.Day = as.numeric(al_od_filterred$No.of.Subs.On.21st.Day)
al_od_filterred$No.of.Subs.On.28th.Day..28TH_DAY = as.numeric(al_od_filterred$No.of.Subs.On.28th.Day..28TH_DAY)
al_od_filterred$Monthly.Subs.of.the.Channel = as.numeric(al_od_filterred$Monthly.Subs.of.the.Channel)
##add new alacarte from single cas code
al_od_singlecas = bc_odisha_combo %>% filter(X == 'Alacarte') %>% select(Entity.Code,Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
                                                                         No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel)
colnames(al_od_singlecas)[4]<-'Channel'
##join alacarte
al_od_final = rbind(al_od_filterred,al_od_singlecas)
al_od_final = merge(al_od_final,lco_city)

bc_name = al_od_final %>%
  select(Broadcaster.Name,Channel) %>% distinct()%>% na.omit()

active_pivot = al_od_final %>% 
  group_by(City,Channel) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day..14TH_DAY),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day..28TH_DAY),'Average' = sum(Monthly.Subs.of.the.Channel))
od_al_rpt = merge(bc_name,active_pivot)

####WORK ON WB BOUQUET ####
bq_wb = read_xlsx(file.choose(), skip = 1)
names(bq_wb) = make.names(names(bq_wb))
bq_wb = bq_wb[!grepl("DPO Promotional Bundle", bq_wb$Plan.Name),]
##different single cas code pack and calculate
bq_wb_nw_sncd = bq_wb %>% filter(Bouquet %in% bouquet_names$Bouquet) %>% select(Entity.Code,Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
                                                                                No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel)
bq_wb_nw_sncd_7 = bq_wb_nw_sncd %>% select(Entity.Code,Plan.Name,No.of.Subs.On.7th.Day) %>% unique()
bq_wb_nw_sncd_14 = bq_wb_nw_sncd %>% select(Entity.Code,Plan.Name,No.of.Subs.On.14th.Day..14TH_DAY) %>% unique()
bq_wb_nw_sncd_21 = bq_wb_nw_sncd %>% select(Entity.Code,Plan.Name,No.of.Subs.On.21st.Day) %>% unique()
bq_wb_nw_sncd_28 = bq_wb_nw_sncd %>% select(Entity.Code,Plan.Name,No.of.Subs.On.28th.Day..28TH_DAY) %>% unique()
bq_wb_nw_sncd_avg = bq_wb_nw_sncd %>% select(Entity.Code,Plan.Name,Monthly.Subs.of.the.Channel) %>% unique()
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

bq_wb_nw_sncd_combo_bouq = bq_wb_nw_sncd_combo %>% filter(X == 'Bouquet') %>% select(Entity.Code,Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
                                                                                     No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel)
#bq_wb_nw_sncd_combo_bouq = read.csv(file.choose())
####old pacakges working
bc_wb_old = bq_wb %>% filter(!(Bouquet %in% bouquet_names$Bouquet))

bq_wb_old_pack = bc_wb_old[!grepl("_", bc_wb_old$Plan.Name),] %>% select(Entity.Code,Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
                                                                         No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel)
bq_wb_new_pack = bc_wb_old[grepl("_", bc_wb_old$Plan.Name),]
# bqwbnew_7 = bq_wb_new_pack %>% select(Entity.Code,Plan.Name,No.of.Subs.On.7th.Day) %>% unique()
# bqwbnew_14 = bq_wb_new_pack %>% select(Entity.Code,Plan.Name,No.of.Subs.On.14th.Day..14TH_DAY) %>% unique()
# bqwbnew_21 = bq_wb_new_pack %>% select(Entity.Code,Plan.Name,No.of.Subs.On.21st.Day) %>% unique()
# bqwbnew_28 = bq_wb_new_pack %>% select(Entity.Code,Plan.Name,No.of.Subs.On.28th.Day..28TH_DAY) %>% unique()
# bqwbnew_avg = bq_wb_new_pack %>% select(Entity.Code,Plan.Name,Monthly.Subs.of.the.Channel) %>% unique()
# pack_7 = read.csv(file.choose())
# pack_14 = read.csv(file.choose())
# pack_21 = read.csv(file.choose())
# pack_28 = read.csv(file.choose())
# bqwbnew_7_pk = merge(bqwbnew_7,pack_7,all = F) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")
# bqwbnew_14_pk = merge(bqwbnew_14,pack_14,all = F) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")
# bqwbnew_21_pk = merge(bqwbnew_21,pack_21,all = F) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")
# bqwbnew_28_pk = merge(bqwbnew_28,pack_28,all = F) %>% unique() %>% unite(combined, c('Plan.Name','Bouquet'),sep = "|")
# bqwbnew_combo = merge(bqwbnew_7_pk,bqwbnew_14_pk, all = T)
# bqwbnew_combo = merge(bqwbnew_combo, bqwbnew_21_pk, all = T)
# bqwbnew_combo = merge(bqwbnew_combo, bqwbnew_28_pk, all = T) %>% separate(combined, into = c("Plan.Name","Bouquet"),sep = "\\|")
# bqwbnew_combo[is.na(bqwbnew_combo)] <- 0
# bqwbnew_combo$No.of.Subs.On.7th.Day = as.numeric(bqwbnew_combo$No.of.Subs.On.7th.Day)
# bqwbnew_combo$No.of.Subs.On.14th.Day..14TH_DAY = as.numeric(bqwbnew_combo$No.of.Subs.On.14th.Day..14TH_DAY)
# bqwbnew_combo$No.of.Subs.On.21st.Day = as.numeric(bqwbnew_combo$No.of.Subs.On.21st.Day)
# bqwbnew_combo$No.of.Subs.On.28th.Day..28TH_DAY = as.numeric(bqwbnew_combo$No.of.Subs.On.28th.Day..28TH_DAY)
# bqwbnew_combo = bqwbnew_combo %>% mutate(Monthly.Subs.of.the.Channel = rowMeans(select(bqwbnew_combo, starts_with("No.of"))))
# #bqwbnew_combo = read.csv(file.choose())
# bqwbnew_combo_bouq = bqwbnew_combo %>% filter(X == 'Bouquet') %>% select(Entity.Code,Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
#                                                                          No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel)
wb_bouquet_final = rbind(bq_wb_old_pack,bq_wb_nw_sncd_combo_bouq)
wb_bouquet_final$No.of.Subs.On.7th.Day = as.numeric(wb_bouquet_final$No.of.Subs.On.7th.Day)
wb_bouquet_final$No.of.Subs.On.14th.Day..14TH_DAY = as.numeric(wb_bouquet_final$No.of.Subs.On.14th.Day..14TH_DAY)
wb_bouquet_final$No.of.Subs.On.21st.Day = as.numeric(wb_bouquet_final$No.of.Subs.On.21st.Day)
wb_bouquet_final$No.of.Subs.On.28th.Day..28TH_DAY = as.numeric(wb_bouquet_final$No.of.Subs.On.28th.Day..28TH_DAY)
wb_bouquet_final$Monthly.Subs.of.the.Channel = as.numeric(wb_bouquet_final$Monthly.Subs.of.the.Channel)

wb_bouquet_final = merge(wb_bouquet_final,lco_city)
wbbqt_active_pivot = wb_bouquet_final %>% 
  group_by(City,Bouquet,Broadcaster.Name) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day..14TH_DAY),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day..28TH_DAY),'Average' = sum(Monthly.Subs.of.the.Channel))

######## WORK ON WB ALACARTE #####
al_wb = read_xlsx(file.choose(), skip = 1)
names(al_wb) = make.names(names(al_wb))
al_wb = al_wb[!grepl("DPO Promotional Bundle", al_wb$Plan.Name),]
al_wb_filterred = al_wb %>%
  select(Entity.Code,Broadcaster.Name,Plan.Name,Channel,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
         No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel) %>% distinct() %>% na.omit()
al_wb_filterred$No.of.Subs.On.7th.Day = as.numeric(al_wb_filterred$No.of.Subs.On.7th.Day)
al_wb_filterred$No.of.Subs.On.14th.Day..14TH_DAY = as.numeric(al_wb_filterred$No.of.Subs.On.14th.Day..14TH_DAY)
al_wb_filterred$No.of.Subs.On.21st.Day = as.numeric(al_wb_filterred$No.of.Subs.On.21st.Day)
al_wb_filterred$No.of.Subs.On.28th.Day..28TH_DAY = as.numeric(al_wb_filterred$No.of.Subs.On.28th.Day..28TH_DAY)
al_wb_filterred$Monthly.Subs.of.the.Channel = as.numeric(al_wb_filterred$Monthly.Subs.of.the.Channel)
#wb_new_ala = bqwbnew_combo %>% filter(X == 'Alacarte') %>% select(Entity.Code,Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
#                                                                  No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel)
#colnames(wb_new_ala)[4]<-'Channel'
##add from single cascode
al_wb_singlecas = bq_wb_nw_sncd_combo %>% filter(X == 'Alacarte') %>% select(Entity.Code,Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
                                                                            No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel)
colnames(al_wb_singlecas)[4]<-'Channel'
#al_wb_singlecas = read.csv(file.choose())

wb_new_ala_final = rbind(al_wb_filterred,al_wb_singlecas)
wb_new_ala_final = merge(wb_new_ala_final,lco_city)


active_pivot_ala_wb = wb_new_ala_final %>% 
  group_by(City,Channel,Broadcaster.Name) %>%
  summarize('Active_7th' = sum(No.of.Subs.On.7th.Day),'Active_14th' = sum(No.of.Subs.On.14th.Day..14TH_DAY),'Active_21st' = sum(No.of.Subs.On.21st.Day),
            'Active_28th' = sum(No.of.Subs.On.28th.Day..28TH_DAY),'Average' = sum(Monthly.Subs.of.the.Channel))


#### print in excel ####

write.xlsx(as.data.frame(od_bq_rpt), file="Output/Areawise_MSR_Report_all_May23.xlsx", sheetName="Odisha_Bouquet", row.names=FALSE)
write.xlsx(as.data.frame(od_al_rpt), file="Output/Areawise_MSR_Report_all_May23.xlsx", sheetName="Odisha_Alacarte", append=TRUE, row.names=FALSE)
write.xlsx(as.data.frame(wbbqt_active_pivot), file="Output/Areawise_MSR_Report_all_May23.xlsx", sheetName="WB_Bouquet", append=TRUE,row.names=FALSE)
write.xlsx(as.data.frame(active_pivot_ala_wb), file="Output/Areawise_MSR_Report_all_May23.xlsx", sheetName="WB_Alacarte", append=TRUE, row.names=FALSE)

