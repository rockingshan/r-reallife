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
lco_city = lco_city %>% select(Lco.Code,City) %>% unique()
lco_city$Lco.Code <- gsub("'","",lco_city$Lco.Code)
colnames(lco_city)[1] <- "Entity.Code"

singlepack_7 = read.csv(file.choose())
singlepack_14 = read.csv(file.choose())
singlepack_21 = read.csv(file.choose())
singlepack_28 = read.csv(file.choose())
###WORK  BQ REPORT ####
bc_odisha = read_xlsx(file.choose(), skip = 1)
names(bc_odisha) = make.names(names(bc_odisha))
bc_odisha = bc_odisha[!grepl("DPO Promotional Bundle", bc_odisha$Plan.Name),]
bc_odisha = bc_odisha %>% filter(!(Broadcaster.Name %in% c("ABP News Network Pvt Limited","Free to AIR","Republic TV")))

##calculate new count
bc_odisha_nw = bc_odisha %>% filter(Bouquet %in% bouquet_names$Bouquet) %>% select(Entity.Code,Broadcaster.Name,Plan.Name,Bouquet,No.of.Subs.On.7th.Day,No.of.Subs.On.14th.Day..14TH_DAY,
                                                                                   No.of.Subs.On.21st.Day,No.of.Subs.On.28th.Day..28TH_DAY,Monthly.Subs.of.the.Channel)
bc_odisha_nw_7 = bc_odisha_nw %>% select(Entity.Code,Plan.Name,No.of.Subs.On.7th.Day) %>% unique()
bc_odisha_nw_14 = bc_odisha_nw %>% select(Entity.Code,Plan.Name,No.of.Subs.On.14th.Day..14TH_DAY) %>% unique()
bc_odisha_nw_21 = bc_odisha_nw %>% select(Entity.Code,Plan.Name,No.of.Subs.On.21st.Day) %>% unique()
bc_odisha_nw_28 = bc_odisha_nw %>% select(Entity.Code,Plan.Name,No.of.Subs.On.28th.Day..28TH_DAY) %>% unique()
bc_odisha_nw_avg = bc_odisha_nw %>% select(Entity.Code,Plan.Name,Monthly.Subs.of.the.Channel) %>% unique()
#singlecode pak

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
al_od = al_od %>% filter(!(Broadcaster.Name %in% c("ABP News Network Pvt Limited","Free to AIR","Republic TV")))

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


#### print in excel ####

write.xlsx(as.data.frame(od_bq_rpt), file="Output/Areawise_MSR_Report_all_Oct23.xlsx", sheetName="Area_Bouquet", row.names=FALSE)
write.xlsx(as.data.frame(od_al_rpt), file="Output/Areawise_MSR_Report_all_Oct23.xlsx", sheetName="Area_Alacarte", append=TRUE, row.names=FALSE)

###lco code wise
write.csv(bc_odisha_bq_all,"LCOWISE_BOUQUET.csv",row.names = F)
write.csv(al_od_final,"LCOWISE_Alacarte.csv",row.names = F)
