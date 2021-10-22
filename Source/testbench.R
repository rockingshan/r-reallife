library(tidyverse)
library(dplyr)
library(lubridate)
library(janitor)
library(xlsx)

inventory = read.csv(file.choose(new = F),colClasses = c(SERIAL_NUMBER="character")) ##inventory file
customer_data = read.csv(file.choose(new = F)) ##customer master data
#work on chandipur inventory file
inv_cndp = inventory %>% filter(str_detect(ENTITY_CODE, "HCS")) %>% filter(!(str_detect(ITEM_CODE, "SC"))) %>%
  select(SERIAL_NUMBER,TYPE,ITEM_DESCR,LOCATION_DESCR,ENTITY_CODE,CUSTOMER_NBR) %>% unique()

cust_sel = customer_data %>% select(Customer.Number,Created.Date,Customer.Status) %>% unique()

inv_cust_data = merge(inv_cndp,cust_sel, by.x = "CUSTOMER_NBR",by.y = "Customer.Number", all.x = T)                                                                            

write.csv(inv_cust_data,"Output/Inventory_customer.csv",row.names = F)

######Daily disconnection and active customer data
daily_discon = read.csv(file.choose(new = F))
daily_discon_cn = daily_discon %>% filter(str_detect(Entity.Code, "MDCH"))
daily_dis_pv_cn = daily_discon_cn %>% group_by(Entity.Code) %>% summarize(Discon.Count = n()) %>% adorn_totals("row")

activ_cust = read.csv(file.choose(new = F))
active_cust_for_lookup = activ_cust %>% filter(str_detect(Entity.Code, "MDCH")) %>% select(Entity.Code,Entity.Name) %>% unique()
activ_cust = activ_cust %>% filter(str_detect(Entity.Code, "MDCH")) 
active_pivot = activ_cust %>% 
  group_by(Entity.Code) %>%
  summarize(Total_Active = sum(Active.Customer))
active_pivot = merge(active_pivot, active_cust_for_lookup)
active_pivot = active_pivot[, c(1,3,2)] %>% adorn_totals("row")

write.xlsx(as.data.frame(active_pivot), file="Output/Daily_disconnect_active.xlsx", sheetName="Active", row.names=FALSE)
write.xlsx(as.data.frame(daily_dis_pv_cn), file="Output/Daily_disconnect_active.xlsx", sheetName="DailyDiscon", append=TRUE, row.names=FALSE)
