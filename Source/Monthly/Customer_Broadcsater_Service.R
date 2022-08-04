library(tidyverse)
library(dplyr)

list_active = read.csv(file.choose(new = F), skip = 1, header = FALSE, colClasses = c("character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character") ) #import MQ data
colnames(list_active) <- c("CUSTOMER_NBR","CONTRACT_NUMBER","ENTITY_CODE","ENTITY_NAME","LCO_CITY","LCO_STATE","FIRST_NAME","MIDDLE_NAME","LAST_NAME","STB","SC","SERVICE_NAME","SERVICE_CODE","CASCODE","PLAN_CODE","PLAN_NAME","BILLING_FREQUENCY","MOBILE_PHONE","EMAIL","HOME_PHONE","PRI_STATE","PRI_CITY","PRI_ADDRESS1","a12","s2w2","z2wqww","z2asssz","sdwes2","s2111w2","z2wqw111w","z2a111sz","sd322s2")
list_active_flt = list_active %>% filter(SERVICE_CODE != '')
customer_data = list_active_flt %>% select(CUSTOMER_NBR,CONTRACT_NUMBER,ENTITY_CODE,ENTITY_NAME,LCO_CITY,LCO_STATE,FIRST_NAME,MIDDLE_NAME,LAST_NAME,PLAN_NAME,BILLING_FREQUENCY,MOBILE_PHONE,EMAIL,HOME_PHONE,PRI_STATE,PRI_CITY,PRI_ADDRESS1,a12,s2w2,z2wqww,z2asssz,sdwes2,s2111w2,z2wqw111w,z2a111sz,sd322s2) %>% unique()
customer_data_fl = filter(customer_data, PLAN_NAME %in% plan_names$Plan.Name)

service_name = read.csv(file.choose(new = F))
service_name = service_name %>% select(Service.Code,Broadcaster) %>% unique()
colnames(service_name)[1] <- 'SERVICE_CODE'

list_active$STB <- gsub("'","",list_active$STB)
list_active$SC <- gsub("'","",list_active$SC)
colnames(list_active)[10] <- "VC"
colnames(list_active)[11] <- "STB"
list_active_SHORT = list_active %>% select(CUSTOMER_NBR,ENTITY_CODE,LCO_CITY,FIRST_NAME,MOBILE_PHONE,VC,STB,SERVICE_CODE,SERVICE_NAME,PLAN_CODE,PLAN_NAME) %>% filter(PLAN_CODE != "DPOPROMBUN")

active_list_br = merge(list_active_SHORT,service_name)
active_pivot = active_list_br %>% group_by(CUSTOMER_NBR,Broadcaster) %>% summarize(ServiceCount = n()) %>%
  pivot_wider(names_from = Broadcaster,values_from = ServiceCount)
write.csv(active_pivot,"customer_broadcaster.csv",row.names = F)


write.csv(customer_data_fl,"customer_data.csv",row.names = F)
