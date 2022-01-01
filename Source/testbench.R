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

#############################
################################
#Find customers without basic pack in recharge
wallet_list = read.csv(file.choose(new = F)) #import last few days wallet report
plan_names = read.csv(sprintf("https://spreadsheets.google.com/feeds/download/spreadsheets/Export?key=17fLf3_5nMKuOZxMvKY_baJjD3G8l-KKHxw3WSTNKh6o&exportFormat=csv"))
colnames(wallet_list)[8]<-"Plan.Name" #change column name to match online file name
wallet_list_filt = filter(wallet_list, Plan.Name %in% plan_names$Plan.Name) %>% select(Customer.Nbr,Contract.Number,Entity.Code,Plan.Name,Amount.Debit) %>% unique()
wallet_accounts = wallet_list %>% select(Customer.Nbr) %>% unique()
wallet_merge = merge(wallet_accounts,wallet_list_filt,all.x = T)
wallet_na_basic = wallet_merge[is.na(wallet_merge$Amount.Debit),]
wallet_pivot = wallet_list %>% group_by(Customer.Nbr) %>% summarize(Total.debit = sum(Amount.Debit))
write.xlsx(as.data.frame(wallet_na_basic), file="Output/Wallet_basic_check.xlsx", sheetName="NoBasic", row.names=FALSE)
write.xlsx(as.data.frame(wallet_pivot), file="Output/Wallet_basic_check.xlsx", sheetName="TotalDebit", append=TRUE, row.names=FALSE)


#######################
#find hd sd box count
alacarte = read.csv(file.choose(new = F))
bouquet = read.csv(file.choose(new = F))
alacount = alacarte %>% select(Customer.Number,LCO.Code) %>% unique()
bouquetcount = bouquet %>% select(Customer.Number,LCO.Code) %>% unique()
totalcount = rbind(alacount,bouquetcount) %>% unique()
COUNT_PIVOT = totalcount %>% group_by(LCO.Code) %>% summarise(COunt_ac = n())
write.csv(COUNT_PIVOT,"OCT.CSV",row.names = F)


bronze_basic = bouquet %>% filter(Week == 4) %>% select(Customer.Number,Bouquet,Plan.Name) %>% filter(Bouquet == "Bronze Basic") %>% unique()
basic_pivot = bronze_basic %>% group_by(Plan.Name,Bouquet) %>% summarize(Active_count = n())
write.csv(basic_pivot,"4.csv",row.names = F)
