library(tidyverse)
library(dplyr)
library(readxl)

list_active = read_csv(file.choose(new = F)) #list active
pack_cost = read_csv(file.choose(new = F)) #package wise service and their costing
operPrice = read_csv(file.choose(new = F))
lcolist = readxl::read_excel(file.choose(new = F))
plan_name = pack_cost %>% select(PLAN_NAME) %>% unique()
plan_list = plan_name[['PLAN_NAME']]
pack_cost = pack_cost %>% unite(PLANSERVICE, c("PLAN_NAME","SERVICE_NAME"),sep = "_")
list_active_flt = list_active %>% filter(SERVICE_CODE != '') %>% select(CUSTOMER_NBR,CONTRACT_NUMBER,ENTITY_CODE,ENTITY_NAME,LCO_CITY,PLAN_CODE,PLAN_NAME,SERVICE_CODE,SERVICE_NAME)
list_active_pack = list_active_flt %>% filter(PLAN_NAME %in% plan_list)
list_active_pack = list_active_pack %>% unite(PLANSERVICE, c("PLAN_NAME","SERVICE_NAME"),sep = "_")
list_active_pckbill = merge(list_active_pack,pack_cost)
list_active_pckbill = list_active_pckbill %>% separate(PLANSERVICE, into = c("PLAN_NAME", "SERVICE_NAME"), sep = "_", remove = TRUE)
planCosting = list_active_pckbill %>% group_by(CUSTOMER_NBR,ENTITY_CODE,PLAN_CODE,PLAN_NAME,Broadcaster) %>% summarise(Bill.Amount = sum(BILL_AMOUNT)) %>%
  pivot_wider(names_from = Broadcaster,values_from = Bill.Amount) %>% replace(is.na(.), 0) %>% mutate(Total.BC.Bill = rowSums(across(where(is.numeric))))
planCosting = planCosting %>% filter(ENTITY_CODE %in% lcolist$ENTITY_CODE)
planCosting = planCosting %>% unite(LCOPLAN, c("ENTITY_CODE","PLAN_CODE"),sep = "|")
operPrice = operPrice %>% unite(LCOPLAN,c("Entity Code","Plan Code"),sep = "|")
operPrice = operPrice %>% mutate(LCO.Price = Price + (Price*0.18))
operPrice_slct = operPrice %>% select(LCOPLAN,LCO.Price)
active_bill = merge(planCosting,operPrice_slct)
active_bill = active_bill %>% separate(LCOPLAN, into = c("LCO_CODE","PLAN_CODE"), sep = "|", remove = TRUE)

write.csv(active_bill,"asdas.csv")
