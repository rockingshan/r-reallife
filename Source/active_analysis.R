library(tidyverse)
library(here)
library(dplyr)
library(gapminder) 
library(qdapTools)
source('Source/Functions.r')

##opens a window to select files, 
list_active <- mq_active_report()

broadcaster = read.csv(file.choose(new = F))


#d1= list_active[,14] %l% broadcaster
## replace ' in column data, change to proper column names
list_active_flt = list_active %>% filter(SERVICE_CODE != '')

##BROADCASTERWISE DATA
list_active_bc = merge(list_active_flt,broadcaster,by.x="SERVICE_CODE",by.y = 'Service.Code',all.x = T,all.y = F)
list_active_bc <- list_active_bc %>% select(CUSTOMER_NBR,ENTITY_NAME,LCO_CITY,STB,SERVICE_NAME,Broadcaster,MOBILE_PHONE,HOME_PHONE) %>% unique()
list_active_piv = list_active_bc %>% group_by(CUSTOMER_NBR,Broadcaster) %>% summarize(Acc_count = n())


dq = list_active_piv %>% pivot_wider(names_from = Broadcaster, values_from = Acc_count)
list_active_short <- list_active %>% select(CUSTOMER_NBR,ENTITY_NAME,LCO_CITY,STB,VC,MOBILE_PHONE,HOME_PHONE) %>% unique()
write.csv(list_active_short, "list_all.csv", row.names = FALSE)

active_service = list_active %>% group_by(SERVICE_NAME) %>% summarise(Active_count = n())
write.csv(active_service,"Service_count_list_active.csv",row.names = F)

active_cust = list_active_flt %>% select(ENTITY_CODE,CUSTOMER_NBR) %>% unique() %>% group_by(ENTITY_CODE) %>% summarise(Active_cust = n())
write.csv(active_cust, "customer.csv",row.names = F)




















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