library(tidyverse)
library(dplyr)
library(lubridate)
library(janitor)
library(httr)
library(xlsx)
library(RDCOMClient)

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
list_active = list_active %>% filter(!(PLAN_CODE == 'DPOPROMBUN'))
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

#######################  find discre;pencies in dpo plans SERVICE &&&&&&&&&&&&&&&&&&&&&&&&&&&& WISE COUNTS FOR CUSTOMERS  ??????????? This block for planwise servicewise
list_active = read.csv(file.choose(new = F), skip = 1, header = FALSE, colClasses = c("character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL") ) #import MQ data
colnames(list_active) <- c("CUSTOMER_NBR","CONTRACT_NUMBER","ENTITY_CODE","ENTITY_NAME","LCO_CITY","LCO_STATE","FIRST_NAME","MIDDLE_NAME","LAST_NAME","STB","SC","SERVICE_NAME","SERVICE_CODE","CASCODE","PLAN_CODE","PLAN_NAME","BILLING_FREQUENCY","MOBILE_PHONE","EMAIL","HOME_PHONE","PRI_STATE","PRI_CITY","PRI_ADDRESS1")
plan_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=17GoiwT4nWCn0J_7HJF0ZyL5Y0-JPNwOJ&export=download"))
plan_list = plan_names[['Plan.Name']]

for (planname in plan_list) {
active_flt = filter(list_active,PLAN_NAME=="Meghbela Starter Pack @ 185")
actv_flt_pvot = active_flt %>% group_by(CUSTOMER_NBR,CONTRACT_NUMBER,SERVICE_NAME) %>% summarise(Service_Count = n()) %>%
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
discon_data_flt = discon_data_flt %>% filter(discon_data_flt, Set.Top.Box.Number != "No STB")
discon_data_flt <- discon_data_flt[-c(391023),]
discon_data_flt <- discon_data_flt[!(discon_data_flt$DISCONNECTED_DATE_7==""),]
discon_data_flt$DISCONNECTED_DATE_7 = as.Date(discon_data_flt$DISCONNECTED_DATE_7, format('%d-%b-%y'))
discon_dt_max = discon_data_flt %>% group_by(Customer.Number) %>% mutate(Disc.Date = max(DISCONNECTED_DATE_7))
discon_dt_max <- discon_dt_max %>% select(Customer.Number,Smartcard.Number,Prov.Sys.Name,Disc.Date) %>% unique()
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
###########################pacakge cratorr with channels

inventory_BOX = read.csv(file.choose(new = F)) ##PACK WITH SERVICE
inventory_CARD = read.csv(file.choose(new = F)) ##
SAFE_CSCODE = read.csv(file.choose(new = F),colClasses = c(SubscriptionID="character"))
DF_K = merge(inventory_BOX,inventory_CARD,all.x = T)
DF_K$Provsion.Code <- gsub("'","",DF_K$Provsion.Code)
newplan = read.csv(file.choose(new = F))
dpoabv = read.csv(file.choose(new = F)) ##import dpo name and cas codes
DF_K = DF_K %>% mutate(Package = recode(Package,'CLASSIC HINDI @ 300'='CLASSIC_HIN_300','DIAMOND DIGITAL @ 380'='DIAMOND_DIG_380','GOLD DIGITAL @ 353'='GOLD_DIG_353',
                                        'HD DHAMAKA @ 300'='HD_DHA_300','MB BANGLA HD 1 @ 455'='MB_HD_BENG_455','MB BANGLA HD 2 @ 550'='MB_HD_BENG_550','MB HINDI HD 1 @ 455'='MB_HD_HIN_455',
                                        'MB HINDI HD 2 @ 550'='MB_HD_HIN_550','Meghbela Basic Pack @ 155'='Meghbela_Pac_155','Meghbela Bengali Starter @165'='Meghbela_Sta_165',
                                        'Meghbela Bonanza @ 330'='Meghbela_Bon_330','Meghbela Bonanza Rural @ 245'='Meghbela_Rur_245','Meghbela Starter Pack @ 185'='Meghbela_Pac_185',
                                        'ODISHA GOLD DIGITAL @ 200'='ODISHA_DIG_200','ODISHA ROYAL @ 185'='ODISHA_ROY_185','PLATINUM DIGITAL @ 450'='PLATINUM_DIG_450',
                                        'RURAL PACK @ 263'='RURAL_PAC_263','SILVER DIGITAL PLUS- URBAN @ 270'='SILVER_URB_270','SILVER DIGITAL PLUS-RURAL @ 230'='SILVER_RUR_230',
                                        'Silver Digital Power @ 276'='Silver_Pow_276','ODISHA ELITE @170'='ODISHA_ELI_170','ODISHA POWER @ 200'='ODISHA_POW_200','ODISHA SILVER DIGITAL @ 150'='ODISHA_DIG_150'
                                        ))
DF_K_new = merge(DF_K,newplan,all.x = F,all.y = T)
plan_name = DF_K %>% select(Package) %>% unique()
plan_list = plan_name[['Package']]
pck_srv_safe = filter(DF_K,Prov.Sys.Name == 'SAFEVIEW')
pck_srv_gosp = filter(DF_K,Prov.Sys.Name == 'GOSPELL')
pck_srv_abv = filter(DF_K,Prov.Sys.Name == 'ABV')
#special block
plannew = read.csv(file.choose(new = F),colClasses = c(Provision.Code="character"))
planSafeviewChannel = merge(plannew,SAFE_CSCODE,all.x = T,by.x = 'Provision.Code',by.y = 'SubscriptionID') %>% select(Plan.Name,CHANNEL.NAME) %>% unique()
planSafeviewChannel = planSafeviewChannel %>% arrange(CHANNEL.NAME)
planSafeviewChannel = planSafeviewChannel %>% arrange(Package) ### sort data
for (planname in plan_list) {
  pq_flt = filter(planSafeviewChannel,Package==planname)
  write.csv(pq_flt,sprintf("Output/%s.csv",planname),row.names = F)
}

abv_pack = read_excel(file.choose(new = F),skip = 1)
planABVChannel = merge(dpoabv,abv_pack,all.x = T,by.x = 'Provsion.Code',by.y = 'PACKAGEID') %>% select(Plan.Name,CHANNELID,CHANNELNAME) %>% unique()
planABVChannel = planABVChannel %>% arrange(CHANNELID)
planABVChannel = planABVChannel %>% arrange(Package) ### sort data
for (planname in plan_list) {
  pq_flt = filter(planABVChannel,Package==planname)
  write.csv(pq_flt,sprintf("Output/%s.csv",planname),row.names = F)
}
pck_srv_gosp= read.csv(file.choose(new = F))
GSPL_PACK  = read_excel(file.choose(new = F))
planGSPLChannel = merge(pck_srv_gosp,GSPL_PACK,all.x = T,by.x = 'Provsion.Code',by.y = 'Product ID') %>% select('Package','Service ID','Service Name') %>% unique()
planGSPLChannel = planGSPLChannel %>% arrange(ServiceID)
planGSPLChannel = planGSPLChannel %>% arrange(Package) ### sort data
for (planname in plan_list) {
  pq_flt = filter(planGSPLChannel,Package==planname)
  write.csv(pq_flt,sprintf("Output/%s.csv",planname),row.names = F)
}

##nagra block
dponagra = read.csv(file.choose(new = F))
nagrasrvc = read.csv(file.choose(new = F))
dpo_nagra_srv = merge(dponagra,nagrasrvc,all.x = T)

write.csv(planGSPLChannel,"SingleCasCode fro DPO - Gospell.csv")

######################################################
list_active = read.csv(file.choose(new = F))
list_active$STB <- gsub("'","",list_active$STB)
list_active$SC <- gsub("'","",list_active$SC)
colnames(list_active)[10] <- "VC"
colnames(list_active)[11] <- "STB"
list_active <- list_active %>% mutate(VC.length = nchar(VC),  .after = 11) # get character length of vc
list_active$VC.length <- gsub("8","GOSPELL",list_active$VC.length, fixed = TRUE)
list_active$VC.length <- gsub("12","SAFEVIEW",list_active$VC.length, fixed = TRUE)
list_active$VC.length <- gsub("16","ABV",list_active$VC.length, fixed = TRUE) #REPLACE LENGTHS TO CAS NAMES

req_d = list_active %>% filter(Is.Auto.Renew == 'Y') %>% filter(VC.length == 'GOSPELL') %>% select(Customer.Nbr,VC) %>% unique()


################GOSPELL EMMM CORRECTION

gos_emm = read.csv(file.choose(new = F))
gos_1 = gos_emm %>% filter(Type == 'Entitlement') %>% select(CardID,StartTime) %>% unique()

#############finding services
odbq = list_active %>% filter(SERVICE_NAME == 'Odisha Tv Bouqet 1')
write.csv(odbq,"odishatv.csv",row.names = F)
DF = list_active %>% filter(SERVICE_NAME == 'Aaj Tak @ 0.75')

###autorenewal customers find 
ls12 = read.csv(file.choose())
lsac = ls12 %>% filter(Is.Auto.Renew == "Y") %>% select(Customer.Nbr,Contract.Number,Entity.Code,Entity.Name,First.Name,Stb,Sc,Mobile.Phone) %>% unique()
duernw = read.csv(file.choose())
duelist = duernw %>% select(Customer.Number,Contract.Number,Contract.End.Date)
duelist$Contract.End.Date = as.Date(duelist$Contract.End.Date, "%d/%m/%Y")
fnlist = merge(lsac,duelist,by.x = "Customer.Nbr",by.y = "Customer.Number",all.x = T) %>% filter(Contract.End.Date == today())

# Loop over each row of the data frame and make an HTTP request for each customer
for (i in 1:nrow(fnlist)) {
  # Create the request body for the HTTP request using the customer's account number, mobile number, and type
  body <- paste0("<REQUESTINFO>\r\n<KEY_NAMEVALUE>\r\n<KEY_NAME>CONTRACTNO</KEY_NAME>\r\n<KEY_VALUE>", fnlist[i, "Contract.Number.x"], "</KEY_VALUE>\r\n</KEY_NAMEVALUE>\r\n</REQUESTINFO>")
  
  # Create the headers for the HTTP request
  headers <- c(
    'USERNAME' = 'MB102',
    'PASSWORD' = 'Shan4935',
    'EXTERNALPARTY' = 'MQS',
    'Content-Type' = 'application/xml'
  )
  
  # Generate a random reference number and replace the hardcoded value in the URL with it
  ref_no <- paste0(format(runif(1, 510000, 100000000), scientific = FALSE), "a2swzzd3")
  url <- paste0("https://meghbela-bcrm.magnaquest.com/RestService/RestService.svc/RenewContract?referenceno=", ref_no)
  
  # Make the HTTP request using the POST method, the request URL, the request body, and the headers
  res <- VERB("POST", url = url, body = body, add_headers(headers))
  
  # Print the response to the console
  cat(content(res, 'text'))
  
}

#write.csv(fnlist,sprintf("Output/autorenew_%s.csv",today()),row.names = F)



HCSA004 = ls12 %>% filter(Entity.Code == "HCSA004") %>% filter()


######
w1 = read.csv(file.choose())
w2 = read.csv(file.choose())
w3 = read.csv(file.choose())
w4 = read.csv(file.choose())
wall = rbind(w1,w2,w3,w4)
wall = wall %>% relocate(Channel.Name, .after = Broadcaster.Name)
bqall = read.csv(file.choose())
colnames(wall)[6] <- "Bouquet/Channel"
colnames(bqall)[6] <- "Bouquet/Channel"
all_d = rbind(bqall,wall)
write.csv(all_d, "Bouquet_Alacaret_feb23.csv",row.names = F)

#### find services as alacarte with pack ####
list_active = read.csv(file.choose(new = F))

list_service = list_active %>% filter(PLAN_CODE == "ALACARTEPL") %>% filter(SERVICE_CODE %in% c("CH82","CH25")) %>% select(CUSTOMER_NBR,CONTRACT_NUMBER,SERVICE_NAME) %>% unique()
plan_customer = list_active %>% filter(PLAN_CODE == "SLVBUDGDGL") %>% select(CUSTOMER_NBR,ENTITY_CODE,ENTITY_NAME,PLAN_NAME) %>% unique()
custServiceInPlan = merge(plan_customer,list_service,all = F)
custServiceInPlanPivot = custServiceInPlan %>% group_by(SERVICE_NAME) %>% summarise(AlacartCount = n())
oldList_active = read.csv(file.choose(new = F))
oldList_185 = oldList_active %>% filter(PLAN_CODE == "SLVBUDGDGL") %>% select(CUSTOMER_NBR,ENTITY_CODE,ENTITY_NAME) %>% unique()
missingCustomer = merge(oldList_185,plan_customer,all.x = T) %>% filter(is.na(PLAN_NAME))
plan_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=17GoiwT4nWCn0J_7HJF0ZyL5Y0-JPNwOJ&export=download"))
plan_names = plan_names %>% add_row(Plan.Name = "Bronze basic")
newCustPlan = list_active %>% filter(PLAN_NAME %in% plan_names$Plan.Name ) %>% select(CUSTOMER_NBR,PLAN_CODE,PLAN_NAME) %>% unique()
missingCustPlan = merge(missingCustomer,newCustPlan, by = "CUSTOMER_NBR", all.x = T,all.y = F)
missingCustPlanPivot = missingCustPlan %>% group_by(PLAN_NAME.y) %>% summarise(PlanCount = n())

######find new plans ####
ls_new = subset(list_active, grepl('^MBIL',list_active$PLAN_CODE,ignore.case = T))
plan_count = ls_new %>% group_by(ENTITY_CODE) %>% summarise(DPOCOUNT = n())
write.csv(plan_count,"dpo_count.csv")
old_bq = read.csv(file.choose())
ls_old_bq = list_active %>% filter(PLAN_CODE %in% old_bq$PLAN_CODE) 
wallet = read.csv(file.choose())
wallet$Bill.Charge.Start.Date <- parse_date_time(wallet$Bill.Charge.Start.Date, orders = "dmy HMS")
wallet$Bill.Charge.Start.Date <- as.Date(wallet$Bill.Charge.Start.Date)
wallet_ap = wallet %>% filter(Bill.Charge.Start.Date > "2023-03-31")
ls_oldbq_wa = merge(ls_old_bq,wallet_ap,by.x = 'CONTRACT_NUMBER',by.y = 'Contract.Number')
ls_new_serv = subset(ls_new, !(grepl('^MBIL',ls_new$SERVICE_CODE,ignore.case = T)))
write.csv(ls_new_serv,"fj.csv")


###FIND CHANNEL DETAILS ####
channel = read.csv(file.choose())
chnl_act = merge(list_active,channel)
write.csv(chnl_act,"channel_details.csv",row.names = F)
####FIND NEW BC BOUQUETS####
new_pack = read.csv(file.choose())
new_bq = list_active %>% filter(PLAN_CODE %in% new_pack$PLAN_CODE)
write.csv(new_bq,"new_bc_bouquets.csv",row.names = F)


###finding old pack and calculate amount based on remaining date ####
duernw = read.csv(file.choose())
duernw_fl = duernw %>% select(Contract.Number,Contract.End.Date)
planOld = read.csv(file.choose())
price = read.csv(file.choose()) %>% unite(combined, c("Entity.Code","Plan.Code")) %>% filter(Lco.Price.Status == "A") %>% select(combined,Price) %>% unique()
plan_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=17GoiwT4nWCn0J_7HJF0ZyL5Y0-JPNwOJ&export=download"))
list_act_old_bq = list_active %>% filter(PLAN_NAME %in% plan_names$Plan.Name)
list_act_old_bq = subset(list_act_old_bq, !(grepl('^MBIL',list_act_old_bq$PLAN_CODE,ignore.case = T))) %>% select(CUSTOMER_NBR,CONTRACT_NUMBER,ENTITY_CODE,ENTITY_NAME,
                                                                                                                  FIRST_NAME,MOBILE_PHONE,VC,PLAN_CODE,PLAN_NAME,BILLING_FREQUENCY) %>% unique()
list_act_old_bq = merge(list_act_old_bq,duernw_fl,all.x = T,by.x = "CONTRACT_NUMBER",by.y = "Contract.Number") %>% unite(combined,c("ENTITY_CODE","PLAN_CODE"),remove = F) %>% mutate(Type = "DPO")

list_act_bc_bq = list_active %>% filter(PLAN_CODE %in% planOld$PLAN_CODE) %>% select(CUSTOMER_NBR,CONTRACT_NUMBER,ENTITY_CODE,ENTITY_NAME,
                                                                                     FIRST_NAME,MOBILE_PHONE,VC,PLAN_CODE,PLAN_NAME,BILLING_FREQUENCY) %>% unique()
list_act_bc_bq = merge(list_act_bc_bq,duernw_fl,all.x = T,by.x = "CONTRACT_NUMBER",by.y = "Contract.Number") %>% unite(combined,c("ENTITY_CODE","PLAN_CODE"),remove = F) %>% mutate(Type = "Bouquet")
list_act_old = rbind(list_act_old_bq,list_act_bc_bq)
list_act_old_price = merge(list_act_old,price,all.x = T)
list_act_old_price$Contract.End.Date = as.Date(list_act_old_price$Contract.End.Date,  "%d/%m/%Y")
list_act_old_price_cal = list_act_old_price %>% mutate(RefundAmount = ((Price/30)*as.numeric((Contract.End.Date - today())+1)))
write.csv(list_act_old_price_cal,"old_pack_price_calc.csv",row.names = F)

####old bouq all find and refund
old_pk_rem = read.csv(file.choose())
old_pk_act = merge(list_active,old_pk_rem)
write.csv(old_pk_act,"old_pk_act.csv")


####REMOVE OLD BOUQUETS VIA MQ API####

custListSlct = read.csv(file.choose())
# Start timing the execution of the for loop
start_time <- Sys.time()
# Loop over each row of the data frame and make an HTTP request for each customer
for (i in 1:nrow(custListSlct)) {
  # Create the request body for the HTTP request using the customer's account number, mobile number, and type
  body <- paste0("<REQUESTINFO>\r\n<CONTRACTINFO>\r\n <CONTRACTNO>", custListSlct[i, "CONTRACT_NUMBER"], "</CONTRACTNO>\r\n <ORDERDATE>29/05/2023</ORDERDATE>\r\n <EFFECTIVEDATE>29/05/2023</EFFECTIVEDATE>\r\n <BILLFREQUENCY>30D</BILLFREQUENCY>\r\n <SALESMANCODE></SALESMANCODE>\r\n <OUTLETS></OUTLETS>\r\n <NOTES>REMOVE</NOTES>\r\n <STATUS></STATUS>\r\n<DELETEINFO>\r\n<PLANCODE>", custListSlct[i, "PLAN_CODE"], "</PLANCODE>\r\n<SERVICEGROUPCODE></SERVICEGROUPCODE>\r\n<SERVICECODE></SERVICECODE>\r\n<PACKAGEGROUPCODE></PACKAGEGROUPCODE>\r\n<PACKAGECODE></PACKAGECODE>\r\n</DELETEINFO>\r\n<FLEX-ATTRIBUTE-INFO>\r\n<ATTRIBUTE1></ATTRIBUTE1>\r\n<ATTRIBUTE2></ATTRIBUTE2>\r\n<ATTRIBUTE3></ATTRIBUTE3>\r\n<ATTRIBUTE4></ATTRIBUTE4>\r\n<ATTRIBUTE5></ATTRIBUTE5>\r\n<ATTRIBUTE6></ATTRIBUTE6>\r\n<ATTRIBUTE7></ATTRIBUTE7>\r\n<ATTRIBUTE8></ATTRIBUTE8>\r\n<ATTRIBUTE9></ATTRIBUTE9>\r\n<ATTRIBUTE10></ATTRIBUTE10>\r\n</FLEX-ATTRIBUTE-INFO>\r\n</CONTRACTINFO>\r\n</REQUESTINFO>")
  
  # Create the headers for the HTTP request
  headers <- c(
    'USERNAME' = 'MB102',
    'PASSWORD' = 'Shan8648',
    'EXTERNALPARTY' = 'MQS',
    'Content-Type' = 'application/xml'
  )
  
  # Generate a random reference number and replace the hardcoded value in the URL with it
  date <- Sys.time()
  ref_no <- paste0(format(date, format = "%d%m%Y%H%M%S"), "ABRTEY")
  url <- paste0("https://meghbela-bcrm.magnaquest.com/RestService/RestService.svc/ModifyContract?referenceno=", ref_no)
  
  # Make the HTTP request using the POST method, the request URL, the request body, and the headers
  res <- VERB("POST", url = url, body = body, add_headers(headers))
  
  # Print the response to the console
  cat(content(res, 'text'))
  
  # Pause for 5 seconds before making the next HTTP request
  Sys.sleep(1)
  cat("/")
}

# End timing the execution of the for loop
end_time <- Sys.time()

# Print the elapsed time taken to execute the for loop
cat("Elapsed time:", end_time - start_time, "\n")

#####Send mail to LCOs####
# Read in the data
data <- read.csv(file.choose())

reseller_email_map <- read.csv(file.choose())

# loop through each unique reseller code and send an email to the corresponding email address
  
####find alacarte customers of broadcasters####
pack = read.csv(file.choose())
pack_bc = pack %>% select(Service.Code,Broadcaster) %>% unique()
list_alacarte = list_active %>% filter(PLAN_CODE == "ALACARTEPL") %>% select(CUSTOMER_NBR,CONTRACT_NUMBER,ENTITY_CODE,ENTITY_NAME,LCO_CITY,SERVICE_CODE,SERVICE_NAME,PLAN_NAME)
alacarte_bc = merge(list_alacarte,pack_bc, by.x = 'SERVICE_CODE',by.y = 'Service.Code',all.x = T)
##msm
alacarte_bc_msm = alacarte_bc %>% filter(Broadcaster == "Sony Pictures Networks India Pvt. Ltd.")
write.csv(alacarte_bc_msm,"msm_ala_.csv")

####FIND BLANK INVENTORY ####
inventory = read.csv(file.choose(new = F),colClasses = c(SERIAL_NUMBER="character")) ##inventory file
cardtype = c('Gospell SC')
boxtype = c('GOSPELL HD 7601U','GOSPELL HD L316','GOSPELL K925','Gospell STB','GOSPELL SK9501')
inventory_CARD = inventory %>% filter(ITEM_DESCR %in% cardtype) %>% 
  select(SERIAL_NUMBER,TYPE,ITEM_CODE,ITEM_DESCR,LOCATION_DESCR,ENTITY_CODE,CUSTOMER_NBR) %>% filter(TYPE == "Entity") %>% select(ENTITY_CODE,ITEM_CODE,ITEM_DESCR,SERIAL_NUMBER) %>% unique()
colnames(inventory_CARD)[4] <- "CARD_NO"
inventory_BOX = inventory %>% filter(ITEM_DESCR %in% boxtype) %>% 
  select(SERIAL_NUMBER,TYPE,ITEM_CODE,ITEM_DESCR,LOCATION_DESCR,ENTITY_CODE,CUSTOMER_NBR) %>% filter(TYPE == "Entity") %>% select(ENTITY_CODE,ITEM_CODE,ITEM_DESCR,SERIAL_NUMBER) %>% unique()
colnames(inventory_BOX)[4] <- "BOX_NO"
inv_final = merge(inventory_BOX,inventory_CARD,by = "ENTITY_CODE",all.x = T)
blank_cus = read.csv(file.choose())

# Create an empty final table data.frame
final_table <- data.frame(ENTITY_CODE = character(),
                          BOX_NO = character(),
                          CARD_NO = character(),
                          CUSTOMER_NBR = character(),
                          stringsAsFactors = FALSE)

# Iterate over unique LCO_CODE values in df1
unique_lco_codes <- unique(inventory_BOX$ENTITY_CODE)

# Iterate over unique ENTITY_CODE values
for (ENTITY_CODE in unique_lco_codes) {
  # Subset data based on the current ENTITY_CODE
  inventory_BOX_subset <- inventory_BOX[inventory_BOX$ENTITY_CODE == ENTITY_CODE, , drop = FALSE]
  inventory_CARD_subset <- inventory_CARD[inventory_CARD$ENTITY_CODE == ENTITY_CODE, , drop = FALSE]
  blank_cus_subset <- blank_cus[blank_cus$ENTITY_CODE == ENTITY_CODE, , drop = FALSE]
  
  # Check if there are any card numbers available for the current LCO_CODE
  if (nrow(inventory_CARD_subset) == 0) {
    # Skip the current LCO_CODE if there are no card numbers
    next
  }
  if (nrow(inventory_BOX_subset) == 0) {
    # Skip the current LCO_CODE if there are no box numbers
    next
  }
  if (nrow(blank_cus_subset) == 0) {
    # Skip the current LCO_CODE if there are no box numbers
    next
  }
  
  # Get the number of rows for each subset
  n_rows1 <- nrow(inventory_BOX_subset)
  n_rows2 <- nrow(inventory_CARD_subset)
  n_rows3 <- nrow(blank_cus_subset)
  
  # Determine the number of iterations
  n_iter <- max(n_rows1, n_rows2, n_rows3)
  
  # Create a new data.frame for the current ENTITY_CODE
  new_rows <- data.frame(ENTITY_CODE = rep(ENTITY_CODE, n_iter),
                         BOX_NO = inventory_BOX_subset$BOX_NO[1:n_iter],
                         CARD_NO = NA,
                         CUSTOMER_NBR = NA,
                         stringsAsFactors = FALSE)
  
  # Fill in the CARD_NO values from inventory_CARD_subset
  new_rows$CARD_NO[1:n_rows2] <- inventory_CARD_subset$CARD_NO
  
  # Fill in the CUSTOMER_NBR values from blank_cus_subset into new_rows
  new_rows$CUSTOMER_NBR[1:n_rows3] <- blank_cus_subset$CUSTOMER_NBR
  
  # Append the new rows to the final table
  final_table <- rbind(final_table, new_rows)
}

# Replace NA with blank
final_table$CARD_NO[is.na(final_table$CARD_NO)] <- ""
final_table$CUSTOMER_NBR[is.na(final_table$CUSTOMER_NBR)] <- ""
final_table_full = merge(final_table,select(inventory_BOX,ITEM_CODE,BOX_NO),all.x = T) 

# Print the final table
write.csv(final_table_full,"BLANK_STB_VC.csv",row.names = F)

####Sushanta das data####
cust_data = read.csv(file.choose(new = F),colClasses = c(Stb="character",Vc="character"))
cust_data_flt = cust_data %>% filter(Is.Auto.Renewal == 'Y') %>% filter(Customer.Status == 'A') %>%
  select(Customer.Number,First.Name,Last.Name,Stb,Vc,Mobile,Address)
duernw = read.csv(file.choose())
duernw_fl = duernw %>% select(Customer.Number,Contract.End.Date,Plan.Name)
fnl_data = merge(cust_data_flt,duernw_fl,all.x = T)
write.csv(fnl_data,"MD0022_2.CSV",row.names = F)



####mq log work####
log = read.csv(file.choose())

