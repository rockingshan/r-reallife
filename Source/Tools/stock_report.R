library(tidyverse)
library(dplyr)

inventory = read.csv(file.choose(new = F),colClasses = c(SERIAL_NUMBER="character")) ##inventory file
customer_data = read.csv(file.choose(new = F)) ##customer master data
#lcoarea = read.csv(file.choose(new = F))
####work on all area inventory file

cardtype = c('Smart Card','ABV Smart Card','Sumavision SC','Gospell SC','Safeview SC','Nagra Cardless STB','Conax SC')
boxtype = c('MYBOX S427 SFVW','AGGRRESSIVE 5197','AGGRESSIVE 7C02 HD','SMT 3715 HD','AGGRESSIVE 5043 HD','Safeview STB','SMT 3281','ABV STB','SMT 3202','GOSPELL HD 7601U',
            'GOSPELL HD L316','GOSPELL K925','Gospell STB','GOSPELL SK9501','MYBOX H424 HD','MYBOX S427','Aggressive 3711 HD ABV','OVT HD 7C02','D LINK M4','JIUZHOU DTM 7C02 HD',
            'Aggressive 7611 HD','Aggressive 7649 HD ABV','Aggressive 7613 HD ABV','Aggressive 7629','OVT MPEG4 5C35','Nagra Cardless STB')
inventory_all = inventory %>% filter(!(ITEM_DESCR %in% cardtype)) %>% 
  select(SERIAL_NUMBER,TYPE,ITEM_CODE,ITEM_DESCR,LOCATION_DESCR,ENTITY_CODE,CUSTOMER_NBR) %>% unique()

cust_sel = customer_data %>% select(Customer.Number,Created.Date,Customer.Status) %>% unique()
lco_mas = read.csv(file.choose(new = F)) #lco master
lco_mas$Lco.Code <- gsub("'","",lco_mas$Lco.Code)
lco_mas <- lco_mas %>% select(Lco.Code,Business.Name,City,Mobile.Phone,Email)
inv_cust_data = merge(inventory_all,cust_sel, by.x = "CUSTOMER_NBR",by.y = "Customer.Number", all.x = T)  
inv_cust_data[inv_cust_data == ""] <- NA
inv_cust_data$Customer.Status <- as.character(inv_cust_data$Customer.Status)
inv_cust_data$Customer.Status <- gsub("A","Active",inv_cust_data$Customer.Status)
inv_cust_data$Customer.Status <- gsub("I","Inactive",inv_cust_data$Customer.Status)
inv_cust_data$Customer.Status <- gsub("N","Inactive",inv_cust_data$Customer.Status)
inv_cust_data$Customer.Status[is.na(inv_cust_data$Customer.Status)] <- 'In LCO Store'
inv_cust_pivot = inv_cust_data %>% group_by(ENTITY_CODE,Customer.Status) %>% summarise(STB.Count = n()) %>%
  pivot_wider(names_from = Customer.Status,values_from = STB.Count)
inv_cust_pivot[is.na(inv_cust_pivot)] <- 0
inv_cust_pivot = inv_cust_pivot %>% mutate(Total = rowSums(across(where(is.numeric))))
inv_cust_pivot = merge(inv_cust_pivot,lco_mas, by.x = 'ENTITY_CODE',by.y = 'Lco.Code')
write.csv(inv_cust_pivot,"Output/inv_cust_pivot_1.csv",row.names = F)
inv_cust_model_pv = inv_cust_data %>% group_by(ENTITY_CODE,ITEM_DESCR,Customer.Status) %>% summarise(STB.Count = n()) %>%
  pivot_wider(names_from = Customer.Status,values_from = STB.Count)
inv_cust_model_pv[is.na(inv_cust_model_pv)] <- 0
inv_cust_model_pv = inv_cust_model_pv %>% mutate(Total = rowSums(across(where(is.numeric))))
inv_cust_model_pv = merge(inv_cust_model_pv,lco_mas, by.x = 'ENTITY_CODE',by.y = 'Lco.Code')
write.csv(inv_cust_model_pv,"Output/inv_cust_piMODELWISE.csv",row.names = F)
##find customers with one hardware

inv_partial = inventory %>% group_by(CUSTOMER_NBR) %>% summarise(Hardware.Count = n())
inv_partial = inv_partial %>% filter(Hardware.Count == 1)
inv_partial = inv_partial %>% ungroup()
inv_partial_dtls = merge(inventory,inv_partial,all.x = F,all.y = T)
write.csv(inv_partial_dtls,"Single_hardware.csv",row.names = F)

##find one lco inactive data
lcoinv = inventory_all %>% filter(ENTITY_CODE == "MDCH106")
lcocust = merge(lcoinv,cust_sel, by.x = "CUSTOMER_NBR",by.y = "Customer.Number", all.x = T)
lcocust[lcocust == ""] <- NA
lcocust$Customer.Status <- as.character(lcocust$Customer.Status)
lcocust$Customer.Status <- gsub("A","Active",lcocust$Customer.Status)
lcocust$Customer.Status <- gsub("I","Inactive",lcocust$Customer.Status)
lcocust$Customer.Status <- gsub("N","Inactive",lcocust$Customer.Status)
lcocust$Customer.Status[is.na(lcocust$Customer.Status)] <- 'In LCO Store'
