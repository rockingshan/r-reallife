library(tidyverse)
library(dplyr)
#library(gapminder) 
#library(qdapTools)
source('Source/Functions.r')

##opens a window to select files, 
list_active <- mq_active_report()

broadcaster = read.csv(file.choose(new = F))


#d1= list_active[,14] %l% broadcaster
## replace ' in column data, change to proper column names
list_active_flt = list_active %>% filter(SERVICE_CODE != '')

##BROADCASTERWISE DATA
list_active_bc = merge(list_active_flt,broadcaster,by.x="SERVICE_CODE",by.y = 'Service.Code',all.x = T,all.y = F)
list_active_bc <- list_active_bc %>% select(CUSTOMER_NBR,PRI_STATE,ENTITY_NAME,LCO_CITY,STB,SERVICE_NAME,Broadcaster,MOBILE_PHONE,HOME_PHONE) %>% unique()
list_active_piv = list_active_bc %>% group_by(CUSTOMER_NBR,Broadcaster) %>% summarize(Acc_count = n())


dq = list_active_piv %>% pivot_wider(names_from = Broadcaster, values_from = Acc_count)
list_active_short <- list_active %>% select(CUSTOMER_NBR,ENTITY_NAME,LCO_CITY,STB,VC,MOBILE_PHONE,HOME_PHONE) %>% unique()
write.csv(list_active_short, "list_all.csv", row.names = FALSE)

active_service = list_active_flt %>% group_by(SERVICE_NAME) %>% summarise(Active_count = n())
write.csv(active_service,"Service_count_list_active.csv",row.names = F)

active_cust = list_active_flt %>% select(ENTITY_CODE,CUSTOMER_NBR) %>% unique() %>% group_by(ENTITY_CODE) %>% summarise(Active_cust = n())
write.csv(active_cust, "customer.csv",row.names = F)


###compare with SMS # RUN LIST_ACT_FLT FROM ABOVE FIRST

list_bouquet_dated = read.csv(file.choose(new = F),colClasses = c(Service.CAS.Code="character")) #import MQ data bouquet
list_alacarte = read.csv(file.choose(new = F),colClasses = c(Service.CAS.Code="character")) #import MQ alacarte details
list_bouquet_dated$Set.Top.Box <- gsub("'","",list_bouquet_dated$Set.Top.Box)
list_alacarte$Set.Top.Box <- gsub("'","",list_alacarte$Set.Top.Box)

ls_bq_com = list_bouquet_dated %>% select(Customer.Number,Set.Top.Box,Service.CAS.Code,Bouquet) %>% unite(combined, c("Set.Top.Box","Service.CAS.Code"))

ls_act_com = list_active_flt %>% select(STB,CASCODE,ENTITY_CODE) %>% unite(combined, c("STB","CASCODE"))

bouquet_diff = merge(ls_bq_com,ls_act_com,all.x = T,all.y = F)
bouquet_diff_flt = bouquet_diff %>% filter(is.na(ENTITY_CODE)) %>% separate(combined,c("STB","cascode"))

write.csv(bouquet_diff_flt, "Bouquets_in_MSR_notin_ListActive.csv",row.names = F)


ls_al_com = list_alacarte %>% select(Customer.Number,Set.Top.Box,Service.CAS.Code,Channel.Name) %>% unite(combined, c("Set.Top.Box","Service.CAS.Code"))

ala_diff = merge(ls_al_com,ls_act_com,all.x = T,all.y = F)
ala_diff_flt = ala_diff %>% filter(is.na(ENTITY_CODE)) %>% separate(combined,c("STB","cascode"))

write.csv(ala_diff_flt, "Alacarte_in_MSR_notin_ListActive.csv",row.names = F)






customer_num = list_bouquet_dated %>% select(Customer.Number,Set.Top.Box) %>% unique() %>% group_by(Customer.Number) %>% summarise(STB.Count = n())
write.csv(customer_num,"Customer numbers with STB count.csv",row.names = F)

#####PLAN COUNT
plan_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=17GoiwT4nWCn0J_7HJF0ZyL5Y0-JPNwOJ&export=download"))
plan_names = plan_names %>% add_row(Plan.Name = c('DD Channels','Bronze basic','Odia FTA'))
ACTV_PLAN = list_active_flt %>% filter(PLAN_NAME %in% plan_names$Plan.Name) %>% select(PLAN_NAME,ENTITY_CODE,ENTITY_NAME,CUSTOMER_NBR,) %>% unique()
ACTV_PLAN_CNT = ACTV_PLAN %>% group_by(PLAN_NAME) %>% summarise(Active.Count = n())
write.csv(ACTV_PLAN_CNT, "PLAN_COUNT_31082022.CSV")
##plan_count with LCO
lco_count = ACTV_PLAN %>% group_by(ENTITY_CODE,ENTITY_NAME,PLAN_NAME) %>% summarise(Active.Count = n())
write.csv(lco_count, "LCOWISE_PLANCOUNT.CSV",row.names = F)
#######cas entitlement

cas_ent = read.csv(file.choose(new = F))
cas_ent$Stb <- gsub("'","",cas_ent$Stb)
cas_ent$Vc <- gsub("'","",cas_ent$Vc)
colnames(cas_ent)[3] <- "VC"
colnames(cas_ent)[4] <- "STB"

ls_act_com = list_active_flt %>% select(CUSTOMER_NBR,VC,SERVICE_CODE,ENTITY_CODE) %>% unite(combined, c("VC","SERVICE_CODE"))
cas_ent_com = cas_ent %>% select(Customer.Nbr,VC,Service.Code,Prov.System.Name) %>% unite(combined, c("VC","Service.Code"))

cas_diff = merge(ls_act_com,cas_ent_com, all.x = T)

not_in_ent_rep = cas_diff %>% filter(is.na(Prov.System.Name)) %>% separate(combined,c('VC','SERVICE_CODE'))
write.csv(not_in_ent_rep, "Not in cas entitle report.csv", row.names = F)



list_export_cond = filter(list_active, ENTITY_CODE == "MDBKT41")
list_export_cond = list_export_cond %>% 
  select(CUSTOMER_NBR,MOBILE_PHONE,PRI_ADDRESS1,BIL_STATE,BIL_CITY) %>%
  distinct()

list_export_cond = list_active %>% 
  select(CUSTOMER_NBR,ENTITY_CODE,ENTITY_NAME,MOBILE_PHONE) %>% 
  distinct() #select only these column and then remove duplicates

active_pivot = list_export_cond %>% group_by(PLAN_NAME) %>%
  summarize(Plan_Count = n()) # make pivot table

# list_bpc = list_active %>% filter(str_detect(ENTITY_CODE, "MSW")) %>%
#   select(CUSTOMER_NBR,STB,MOBILE_PHONE,HOME_PHONE,ENTITY_CODE,ENTITY_NAME) %>% unique()
# write.csv(list_bpc, "Berhampore_customer.CSV", row.names = FALSE)

#head(list_active
#wallet_filtered = filter(wallet, Plan.Details=="Alacarte Plan")
#target = c("Star Jalsha @ 19", "Jalsha Movies @ 6")
#wallet_filter_condition = filter(wallet_filtered, Service.Name %in% target)
#export_condition = wallet_filter_condition %>% select(Customer.Nbr,Customer.Name,Unique.Id,Entity.Code,Entity.Name,Mobile,Service.Name)
write.csv(list_export_cond, "MDBKT41.csv", row.names = FALSE)



##############list active to broadcaster count - run top block first
lsactv_bc_fl = list_active_bc %>% filter(Broadcaster == 'Star India Pvt. Ltd.')
lst_pck = lsactv_bc_fl %>% select(PLAN_NAME,SERVICE_NAME,PRI_STATE,CUSTOMER_NBR,Broadcaster) %>% unique()
LST_PCK_PIVOT = lst_pck %>% group_by(PRI_STATE,PLAN_NAME,SERVICE_NAME,Broadcaster) %>% summarise(Active.count = n())
