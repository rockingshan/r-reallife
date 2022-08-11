library(tidyverse)
library(dplyr)
library(lubridate)
library(janitor)
library(httr)
#library(xlsx)

source('Source/Functions.r')

send_daily_sms()



#work on chandipur inventory file
inv_cndp = inventory %>% filter(str_detect(ENTITY_CODE, "HCS")) %>% filter(!(str_detect(ITEM_CODE, "SC"))) %>%
  select(SERIAL_NUMBER,TYPE,ITEM_DESCR,LOCATION_DESCR,ENTITY_CODE,CUSTOMER_NBR) %>% unique()

cust_sel = customer_data %>% select(Customer.Number,Created.Date,Customer.Status) %>% unique()

inv_cust_data = merge(inv_cndp,cust_sel, by.x = "CUSTOMER_NBR",by.y = "Customer.Number", all.x = T)                                                                            

write.csv(inv_cust_data,"Output/Inventory_customer.csv",row.names = F)


######Daily disconnection and active customer data
daily_discon = read.csv(file.choose(new = F))  ##Daily disconnection file
daily_discon_cn = daily_discon %>% filter(str_detect(Entity.Code, "MDCH")) 
daily_dis_pv_cn = daily_discon_cn %>% group_by(Entity.Code) %>% summarize(Discon.Count = n()) %>% adorn_totals("row")

activ_cust = read.csv(file.choose(new = F)) #statewise active customer
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

#########################
#Make active report for LCO in single row
list_active = read.csv(file.choose(new = F), skip = 1, header = FALSE, colClasses = c("character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL") ) #import MQ data
colnames(list_active) <- c("CUSTOMER_NBR","CONTRACT_NUMBER","ENTITY_CODE","ENTITY_NAME","LCO_CITY","LCO_STATE","FIRST_NAME","MIDDLE_NAME","LAST_NAME","STB","SC","SERVICE_NAME","SERVICE_CODE","CASCODE","PLAN_CODE","PLAN_NAME","BILLING_FREQUENCY","MOBILE_PHONE","EMAIL","HOME_PHONE","PRI_STATE","PRI_CITY","PRI_ADDRESS1")
list_active$STB <- gsub("'","",list_active$STB)
list_active$SC <- gsub("'","",list_active$SC)
colnames(list_active)[10] <- "VC"
colnames(list_active)[11] <- "STB"
list_active = list_active %>% select(CUSTOMER_NBR,CONTRACT_NUMBER,FIRST_NAME,LAST_NAME,VC,STB,SERVICE_NAME,MOBILE_PHONE,PRI_ADDRESS1) %>% unique()
list_act_combine = list_active %>% group_by(CUSTOMER_NBR) %>% summarise(Services=paste(SERVICE_NAME, collapse=","))
list_act_filter = list_active %>% select(CUSTOMER_NBR,CONTRACT_NUMBER,FIRST_NAME,LAST_NAME,VC,STB,MOBILE_PHONE,PRI_ADDRESS1) %>% unique()
total_active = merge(list_act_combine,list_act_filter)
write.csv(total_active, "LCO_active_data.csv", row.names = F)

###########make all customer data for checking city mapping
list_active_1 = list_active %>% select(CUSTOMER_NBR,ENTITY_CODE,ENTITY_NAME,LCO_CITY,LCO_STATE,PRI_STATE,PRI_CITY) %>% unique()
write.csv(list_active_1, "total_active.csv", row.names = F)

list_active_pivot = list_active_1 %>% group_by(ENTITY_CODE,PRI_CITY) %>% summarise(Active.Count = n())
write.csv(list_active_pivot,"lcowise_active.csv",row.names = F)





#########################
list_active = read.csv(file.choose(new = F), skip = 1, header = FALSE, colClasses = c("character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character") ) #import MQ data
colnames(list_active) <- c("CUSTOMER_NBR","CONTRACT_NUMBER","ENTITY_CODE","ENTITY_NAME","LCO_CITY","LCO_STATE","FIRST_NAME","MIDDLE_NAME","LAST_NAME","STB","SC","SERVICE_NAME","SERVICE_CODE","CASCODE","PLAN_CODE","PLAN_NAME","BILLING_FREQUENCY","MOBILE_PHONE","EMAIL","HOME_PHONE","PRI_STATE","PRI_CITY","PRI_ADDRESS1")
list_active_1 = list_active %>% select(V1,V3,V5,V21,V22,V23,V24,V25,V26,V27,V28,V29,V30,V31,V32) %>% unique()
write.csv(list_active_1, "total_active.csv", row.names = F)

#######################  find discre;pencies in dpo plans
list_active = read.csv(file.choose(new = F), skip = 1, header = FALSE, colClasses = c("character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL") ) #import MQ data
colnames(list_active) <- c("CUSTOMER_NBR","CONTRACT_NUMBER","ENTITY_CODE","ENTITY_NAME","LCO_CITY","LCO_STATE","FIRST_NAME","MIDDLE_NAME","LAST_NAME","STB","SC","SERVICE_NAME","SERVICE_CODE","CASCODE","PLAN_CODE","PLAN_NAME","BILLING_FREQUENCY","MOBILE_PHONE","EMAIL","HOME_PHONE","PRI_STATE","PRI_CITY","PRI_ADDRESS1")
plan_names = read.csv(sprintf("https://spreadsheets.google.com/feeds/download/spreadsheets/Export?key=17fLf3_5nMKuOZxMvKY_baJjD3G8l-KKHxw3WSTNKh6o&exportFormat=csv"))
plan_list = plan_names[['Plan.Name']]

for (planname in plan_list) {
active_flt = filter(list_active,PLAN_NAME==planname)
actv_flt_pvot = active_flt %>% group_by(CUSTOMER_NBR,SERVICE_NAME) %>% summarise(Service_Count = n()) %>%
  pivot_wider(names_from = SERVICE_NAME, values_from = Service_Count)
actv_flt_pvot = adorn_totals(actv_flt_pvot, where = c("col"), fill = "-", na.rm = TRUE, name = "Grand Total")
str_replace_all(planname, "[^[:alnum:]]", " ")
write.csv(actv_flt_pvot,sprintf("Output/%s_.csv",planname),row.names = F)
}

########## find discrepencies of active cust with wallet
wallet_d =  read.csv(file.choose(new = F))
wallet_d_base = filter(wallet_d, Plan.Details %in% plan_names$Plan.Name) %>% select(Customer.Nbr, Contract.Number, Plan.Details, Amount.Debit, Transaction.Date) %>% unique()
wallet_d_max = wallet_d_base %>% group_by(Customer.Nbr,Plan.Details) %>% mutate(Max.Date = max(Transaction.Date)) ##find last date of occurenece
wallet_d_max = wallet_d_max %>% select(Customer.Nbr, Contract.Number, Plan.Details, Amount.Debit, Max.Date)
wallet_d_max = wallet_d_max %>% unite(combined, c("Customer.Nbr","Plan.Details"))
wallet_d_max = wallet_d_max %>% select(combined,Max.Date) %>% unique()
active_c_flt = filter(list_active, PLAN_NAME %in% plan_names$Plan.Name) %>% select(CUSTOMER_NBR,PLAN_NAME,SC,BILLING_FREQUENCY) %>% unique()
active_c_flt = active_c_flt %>% unite(combined, c("CUSTOMER_NBR","PLAN_NAME"))
merge_res = merge(active_c_flt,wallet_d_max, all = T)
write.csv(merge_res, "pack to wallet.csv",row.names = F)

 ##########entitlement
list_bouquet_dated = read.csv(file.choose(new = F)) #import MQ data bouquet

plan_names = read.csv(sprintf("https://spreadsheets.google.com/feeds/download/spreadsheets/Export?key=17fLf3_5nMKuOZxMvKY_baJjD3G8l-KKHxw3WSTNKh6o&exportFormat=csv"))
plan_list = plan_names[['Plan.Name']]

for (planname in plan_list) {
  bq_flt = filter(list_bouquet_dated,Plan.Name==planname)
  bq_pivot = bq_flt %>% group_by(Customer.Number,Bouquet) %>% summarise(Service_Count = n()) %>%
    pivot_wider(names_from = Bouquet, values_from = Service_Count)
  bq_pivot = adorn_totals(bq_pivot, where = c("col"), fill = "-", na.rm = TRUE, name = "Grand Total")
  write.csv(bq_pivot,sprintf("Output/%s_.csv",planname),row.names = F)
}


###################customer manu

customer_data = read.csv(file.choose(new = F)) ##customer master data
customer_select = customer_data %>% select(Customer.Number,Entity.Code,Customer.Status,City,District,Billing.City,Billing.District)
write.csv(customer_select,"customer_fiiltered.csv",row.names = F)

#########################areawise plan details
active_flt = filter(list_active,PLAN_NAME %in% plan_names$Plan.Name)
active_flt = active_flt %>% select(CUSTOMER_NBR,ENTITY_CODE,ENTITY_NAME,PLAN_NAME) %>% unique()
lco_master = read.csv(file.choose(new = F))
active_area = merge(active_flt,lco_master,all.x = T)
write.csv(active_area,"area-active.csv", row.names = F)


#########disconnected summary
discon_data = read.csv(file.choose(new = F))
customer_ent = read.csv(file.choose(new = F))
discon_data_flt = discon_data %>% filter(Smartcard.Number != "No SC")
discon_data_flt = filter(discon_data_flt, Set.Top.Box.Number != "No SC")
discon_data_flt <- discon_data_flt[-c(391023),]
discon_data_flt <- discon_data_flt[!(discon_data_flt$Disconnected.Date==""),]
discon_data_flt$Disconnected.Date = as.Date(discon_data_flt$Disconnected.Date, format('%d-%b-%y'))
discon_dt_max = discon_data_flt %>% group_by(Customer.Number) %>% mutate(Disc.Date = max(Disconnected.Date))
discon_dt_max <- discon_dt_max %>% select(Customer.Number,Disc.Date) %>% unique()
discon_dt_max <- discon_dt_max %>% ungroup()
discon_data_filter = discon_dt_max %>% mutate(Ageing = (lubridate::today() - Disc.Date), .after = NULL )
discon_data_filter <- merge(discon_data_filter,customer_ent)

discon_data_age = discon_data_filter %>% mutate(Ageing.Slab = (ifelse(discon_data_filter$Ageing >= 0 & discon_data_filter$Ageing <= 15, '0-15 days',
                                                                       ifelse(discon_data_filter$Ageing >= 15 & discon_data_filter$Ageing <= 30, '15-30 Days',
                                                                              ifelse(discon_data_filter$Ageing >=30 & discon_data_filter$Ageing <= 60, '30-60 Days',
                                                                                     ifelse(discon_data_filter$Ageing >= 60 & discon_data_filter$Ageing <= 90, '60-90 Days',
                                                                                            ifelse(discon_data_filter$Ageing >= 90 & discon_data_filter$Ageing <= 180, '90-180 Days',
                                                                                            ifelse(discon_data_filter$Ageing >= 180, 'More than 180 Days','6 Months'))))))), .after = NULL)
discon_data_pivot = discon_data_age %>% group_by(Ageing.Slab) %>% summarise(Count.cus = n())
write.csv(discon_data_age, "ageing.csv",row.names = F)



##############################msr to actice tally
list_bouquet_dated = read.csv(file.choose(new = F))
list_bq_cst = list_bouquet_dated %>% select(Customer.Number,Month,Week) %>% unique()
act_cust = list_active %>% select(CUSTOMER_NBR,ENTITY_CODE) %>% unique()
CUST_FILTER = merge(list_bq_cst,act_cust,by.x = 'Customer.Number',by.y = 'CUSTOMER_NBR', all.y = T)
write.csv(CUST_FILTER,"adrfd.csv")

#########################find gospell inactive cardss

list_active = read.csv(file.choose(new = F))
list_active$STB <- gsub("'","",list_active$STB)
list_active$SC <- gsub("'","",list_active$SC)
colnames(list_active)[10] <- "VC"
colnames(list_active)[11] <- "STB"
list_active <- list_active %>% mutate(VC.length = nchar(VC),  .after = 11) # get character length of vc
list_active$VC.length <- gsub("8","GOSPELL",list_active$VC.length, fixed = TRUE)
list_active$VC.length <- gsub("12","SAFEVIEW",list_active$VC.length, fixed = TRUE)
list_active$VC.length <- gsub("16","ABV",list_active$VC.length, fixed = TRUE)
list_active_gs = list_active %>% filter(VC.length == 'GOSPELL') %>% select(VC,CUSTOMER_NBR) %>% unique()
inventory_gs = inventory %>% filter(ITEM_DESCR == 'Gospell SC') %>% select(SERIAL_NUMBER,ITEM_DESCR)
totalGospell = merge(inventory_gs,list_active_gs,by.x = 'SERIAL_NUMBER', by.y = 'STB',all.x = T)
inactiveGospell = totalGospell %>% filter(is.na(CUSTOMER_NBR))
inactiveCount = inactiveGospell %>% group_by(ITEM_DESCR) %>% summarise(count = n())
write.csv(inactiveGospell,"INACTIVE_GOSPELL.CSV",row.names = F)
###########################

df1 = read.csv(file.choose(new = F))
df2 = read.csv(file.choose(new = F))
DF_K = merge(df2,df1,all.x = T)
write.csv(DF_K,"odisha_plans.csv")
