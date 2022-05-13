library(tidyverse)
library(dplyr)
library(lubridate)


activ_cust = read.csv(file.choose(new = F))
active_cust_for_lookup = activ_cust %>% select(Entity.Code,Entity.Name) %>% unique()
active_pivot = activ_cust %>% 
  group_by(Entity.Code) %>%
  summarize(Total_Active = sum(Active.Customer))
active_pivot = merge(active_pivot, active_cust_for_lookup)
active_pivot = active_pivot[, c(1,3,2)]

inventory = read.csv(file.choose(new = F),colClasses = c(SERIAL_NUMBER="character")) ##inventory file
cardtype = c('Smart Card','ABV Smart Card','Sumavision SC','Gospell SC','Safeview SC')
inventory_flt = inventory %>% filter(!(ITEM_DESCR %in% cardtype)) %>% unique()
lco_inventory = inventory_flt %>% group_by(ENTITY_CODE,ITEM_DESCR) %>% summarise(Total.STB = n())
#lco_stb_cust = merge(active_pivot,lco_inventory, by.x = 'Entity.Code', by.y = 'ENTITY_CODE', all.x = T)
write.csv(lco_stb_cust,"Output/lcowise_stb_active.csv",row.names = F)
lco_list = c('MBK001','MBK023','MBK050','MBK051','MBK059','MBK063','MBK065','MBK092','MBK093')
for (lcocode in lco_list) {
  lco_inv_flt = filter(lco_inventory, ENTITY_CODE == lcocode)
  write.csv(lco_inv_flt,sprintf("Output/%s.csv",lcocode),row.names = F)
  
}

########inventorywise customer status

inventory = read.csv(file.choose(new = F),colClasses = c(SERIAL_NUMBER="character")) ##inventory file
customer_data = read.csv(file.choose(new = F)) ##customer master data
lcoarea = read.csv(file.choose(new = F))
####work on all area inventory file

inv_all = inventory %>% filter(!(str_detect(ITEM_CODE, "Smart Card"))) %>% filter(!(str_detect(ITEM_CODE, "SC"))) %>%
  select(SERIAL_NUMBER,TYPE,ITEM_DESCR,LOCATION_DESCR,ENTITY_CODE,CUSTOMER_NBR) %>% unique()

cust_sel = customer_data %>% select(Customer.Number,Created.Date,Customer.Status) %>% unique()

inv_cust_data = merge(inv_all,cust_sel, by.x = "CUSTOMER_NBR",by.y = "Customer.Number", all.x = T)                                                                            
inv_cust_data$Customer.Status <- gsub("A","Active",inv_cust_data$Customer.Status)
inv_cust_data$Customer.Status <- gsub("I","Inactive",inv_cust_data$Customer.Status)
inv_cust_data$Customer.Status <- gsub("N","In LCO Store",inv_cust_data$Customer.Status)
inv_cust_data$Customer.Status <- replace_na('In LCO Store')
inv_data_area = merge(inv_cust_data,lcoarea,all.x = T,all.y = F)
write.csv(inv_data_area,"Output/Inventory_customer.csv",row.names = F)