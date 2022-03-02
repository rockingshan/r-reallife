library(tidyverse)
library(dplyr)
library(lubridate)
library(janitor)
library(httr)
library(xlsx)

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

#########################provision log from sms
pro_log = read.csv(file.choose(new = F))
inventory = read.csv(file.choose(new = F),colClasses = c(SERIAL_NUMBER="character"))
inventory_select = select(inventory, SERIAL_NUMBER,ENTITY_CODE,ITEM_CODE)
colnames(inventory_select)[1] <- "vc"
list_active = read.csv(file.choose(new = F), skip = 1, header = FALSE, colClasses = c("character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL") ) #import MQ data
colnames(list_active) <- c("CUSTOMER_NBR","CONTRACT_NUMBER","ENTITY_CODE","ENTITY_NAME","LCO_CITY","LCO_STATE","FIRST_NAME","MIDDLE_NAME","LAST_NAME","STB","SC","SERVICE_NAME","SERVICE_CODE","CASCODE","PLAN_CODE","PLAN_NAME","BILLING_FREQUENCY","MOBILE_PHONE","EMAIL","HOME_PHONE","PRI_STATE","PRI_CITY","PRI_ADDRESS1")
PRO_LOG_1 = pro_log %>% filter(Prov.System.Name == "GOSPELL",Status =="Disconnected") %>% select(Unique.Id,Provsion.Code) %>% unique()
colnames(PRO_LOG_1) <- c("vc", "cascode")
GSPL_com_log = PRO_LOG_1 %>% unite(combined, c("vc", "cascode"))
list_active$STB <- gsub("'","",list_active$STB)
list_active$SC <- gsub("'","",list_active$SC)
colnames(list_active)[10] <- "VC"
colnames(list_active)[11] <- "STB"
list_active <- list_active %>% mutate(VC.length = nchar(VC),  .after = 11) # get character length of vc
list_active$VC.length <- gsub("8","GOSPELL",list_active$VC.length, fixed = TRUE)
list_ac_GSPL = filter(list_active, VC.length == "GOSPELL")
list_ac_GSPL = list_ac_GSPL %>% unite(combined, c("VC","CASCODE"))
mq_GSPL_data = list_ac_GSPL %>% select(combined,CUSTOMER_NBR,STB,SERVICE_NAME) %>% distinct()
gspl_combine = merge(GSPL_com_log,mq_GSPL_data, all.x = T,all.y = F)
recon_GSPL_NA_output = gspl_combine %>% filter(is.na(CUSTOMER_NBR))
recon_GSPL_NA_output = separate(recon_GSPL_NA_output, combined, c("vc","cascode"))
recon_GSPL_NA_output = select(recon_GSPL_NA_output, vc,cascode)
recon_GSPL_NA_output_ent = merge(recon_GSPL_NA_output,inventory_select, all.x = T)

write.csv(recon_GSPL_NA_output_ent, "Output/Gospell_command log.csv", row.names = F)

#####
gspl_na = read_csv(file.choose(new = F))
gspl_na_fl = gspl_na %>% filter(ENTITY_CODE == "MDBKT04")
write.csv(gspl_na_fl, "Output/Gospell_command_LCOWISE_MDBKT04.csv", row.names = F)


##############send sms - autorenewal\
due_frwn = read.csv(file.choose(new = F))
due_frwn$Mobile.Phone <- as.numeric(due_frwn$Mobile.Phone)
due_frwn <- due_frwn %>% mutate(mob.len = nchar(Mobile.Phone),  .after = 5)
due_frwn <- due_frwn %>% filter(mob.len == 10)
df <- data.frame(matrix(ncol = 2, nrow = 0))
due_frwn$Contract.End.Date <- parse_date_time(due_frwn$Contract.End.Date, orders = "dmy HMS")
due_frwn$Contract.End.Date <- as.Date(due_frwn$Contract.End.Date)
due_frwn_flt = due_frwn %>% filter(Contract.End.Date == lubridate::today()) %>% select(Customer.Number,Mobile.Phone)
for (i in row.names(due_frwn_flt)) {
  readurl = read_lines(paste("http://1.rapidsms.co.in/api/push.json?apikey=60461d4b29af2&route=trans&sender=MCBSPL&mobileno=",due_frwn_flt[i,"Mobile.Phone"],"&text=Dear%20Customer%2C%20Your%20CABLE%20TV%20plan%20for%20account%20",due_frwn_flt[i,"Customer.Number"],"%20is%20expiring%20Today%20.%20Recharge%20now%20to%20avoid%20interruption%20-%20MEGHBELA",sep = ""))
  df[nrow(df) + 1,] = readurl
}

