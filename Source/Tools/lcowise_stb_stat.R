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

