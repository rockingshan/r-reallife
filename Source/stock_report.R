library(tidyverse)
library(dplyr)

inventory = read.csv(file.choose(new = F),colClasses = c(SERIAL_NUMBER="character")) ##inventory file
customer_data = read.csv(file.choose(new = F)) ##customer master data
#lcoarea = read.csv(file.choose(new = F))
####work on all area inventory file

cardtype = c('Smart Card','ABV Smart Card','Sumavision SC','Gospell SC','Safeview SC')
inventory_all = inventory %>% filter(!(ITEM_DESCR %in% cardtype)) %>% 
  select(SERIAL_NUMBER,TYPE,ITEM_DESCR,LOCATION_DESCR,ENTITY_CODE,CUSTOMER_NBR) %>% unique()

cust_sel = customer_data %>% select(Customer.Number,Created.Date,Customer.Status) %>% unique()

inv_cust_data = merge(inventory_all,cust_sel, by.x = "CUSTOMER_NBR",by.y = "Customer.Number", all.x = T)  
inv_cust_data[inv_cust_data == ""] <- NA
inv_cust_data$Customer.Status <- as.character(inv_cust_data$Customer.Status)
inv_cust_data$Customer.Status <- gsub("A","Active",inv_cust_data$Customer.Status)
inv_cust_data$Customer.Status <- gsub("I","Inactive",inv_cust_data$Customer.Status)
inv_cust_data$Customer.Status <- gsub("N","Inactive",inv_cust_data$Customer.Status)
inv_cust_data$Customer.Status[is.na(inv_cust_data$Customer.Status)] <- 'In LCO Store'
inv_cust_pivot = inv_cust_data %>% group_by(ENTITY_CODE,Customer.Status) %>% summarise(STB.Count = n()) %>%
  pivot_wider(names_from = Customer.Status,values_from = STB.Count)
write.csv(inv_cust_pivot,"Output/inv_cust_pivot_1.csv",row.names = F)


##find customers with one hardware

inv_partial = inventory %>% group_by(CUSTOMER_NBR) %>% summarise(Hardware.Count = n())
inv_partial = inv_partial %>% filter(Hardware.Count == 1)
inv_partial = inv_partial %>% ungroup()
inv_partial_dtls = merge(inventory,inv_partial,all.x = F,all.y = T)
write.csv(inv_partial_dtls,"Single_hardware.csv",row.names = F)
