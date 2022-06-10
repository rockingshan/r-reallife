library(tidyverse)
library(dplyr)
library(lubridate)

inventory = read.csv(file.choose(new = F))
activ_cust = read.csv(file.choose(new = F))
new_lco = read.csv(file.choose(new = F))
active_cust_for_lookup = activ_cust %>% select(Entity.Code,Entity.Name) %>% unique()

active_pivot = activ_cust %>% 
  group_by(Entity.Code) %>%
  summarize(Total_Active = sum(Active.Customer))
active_pivot = merge(active_pivot, active_cust_for_lookup)
active_pivot = active_pivot[, c(1,3,2)]

inv_slt = inventory %>% filter(!(str_detect(ITEM_CODE, "SC"))) %>%
  select(SERIAL_NUMBER,TYPE,ITEM_DESCR,LOCATION_DESCR,ENTITY_CODE,CUSTOMER_NBR) %>% unique()
inv_pivot = inv_slt %>% group_by(ENTITY_CODE) %>% summarise(Total_STB = n())
colnames(inv_pivot)[1] <- 'Entity.Code'
lco_active = merge(active_pivot,inv_pivot, all.x = F,all.y = T)
lco_active_new = merge(new_lco,lco_active,all.x = T)
write.csv(lco_active_new,"Output/New_LCO_data.csv",row.names = F)
