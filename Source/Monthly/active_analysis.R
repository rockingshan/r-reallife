library(tidyverse)
library(dplyr)
#library(gapminder) 
#library(qdapTools)
source('Source/Functions.r')

##opens a window to select files, 
list_active <- mq_active_report()

broadcaster = read.csv(file.choose(new = F))
broadcaster = broadcaster %>% select(Service.Code,Broadcaster) %>% unique()

##make servicewise channel
#import package details
pack = read.csv(file.choose(new = F))
pack = pack %>% select(Service.Code,Channel) %>% unique()
#import planwise service
planservice = read.csv(file.choose(new = F))
planservice = planservice %>% select(Service.Code,Service.Name) %>% unique()
service_channel = merge(planservice,pack)

#d1= list_active[,14] %l% broadcaster
## replace ' in column data, change to proper column names
list_active_flt = list_active %>% filter(SERVICE_CODE != '') %>% filter(!(PLAN_NAME == 'DPO Promotional Bundle'))

##BROADCASTERWISE DATA
list_active_bc = merge(list_active_flt,broadcaster,by.x="SERVICE_CODE",by.y = 'Service.Code',all.x = T,all.y = F)
list_active_bc <- list_active_bc %>% select(CUSTOMER_NBR,PRI_STATE,ENTITY_NAME,LCO_CITY,STB,SERVICE_NAME,Broadcaster,MOBILE_PHONE,HOME_PHONE) %>% unique()
list_active_piv = list_active_bc %>% group_by(CUSTOMER_NBR,Broadcaster) %>% summarize(Acc_count = n())


dq = list_active_piv %>% pivot_wider(names_from = Broadcaster, values_from = Acc_count)
list_active_short <- list_active %>% select(CUSTOMER_NBR,ENTITY_NAME,LCO_CITY,STB,VC,MOBILE_PHONE,HOME_PHONE) %>% unique()
write.csv(list_active_short, "list_all.csv", row.names = FALSE)

active_service = list_active_flt %>% group_by(SERVICE_NAME) %>% summarise(Active_count = n())
write.csv(active_service,"Service_count_list_active.csv",row.names = F)

active_cust = list_active_flt %>% select(ENTITY_CODE,CUSTOMER_NBR) %>% unique() %>% group_by(ENTITY_CODE) %>% summarise(Active_cust = n())
write.csv(active_cust, "customer.csv",row.names = F)
#####Broadcaster wise channel value in Alacarte Plan
act_ala = list_active_bc %>% filter(PLAN_NAME == 'Alacarte Plan') %>% select(CUSTOMER_NBR,PRI_STATE,ENTITY_NAME,LCO_CITY,STB,SERVICE_NAME,Broadcaster,MOBILE_PHONE,HOME_PHONE) %>% unique()

###compare with SMS # RUN LIST_ACT_FLT FROM ABOVE FIRST

list_bouquet_dated = read.csv(file.choose(new = F),colClasses = c(Service.CAS.Code="character")) #import MQ data bouquet
list_alacarte = read.csv(file.choose(new = F),colClasses = c(Service.CAS.Code="character")) #import MQ alacarte details
list_bouquet_dated$Set.Top.Box <- gsub("'","",list_bouquet_dated$Set.Top.Box)
list_alacarte$Set.Top.Box <- gsub("'","",list_alacarte$Set.Top.Box)

ls_bq_com = list_bouquet_dated %>% select(Customer.Number,Set.Top.Box,Service.CAS.Code,Bouquet) %>% unite(combined, c("Set.Top.Box","Service.CAS.Code"))

ls_act_com = list_active_flt %>% select(STB,CASCODE,ENTITY_CODE) %>% unite(combined, c("STB","CASCODE"))

bouquet_diff = merge(ls_bq_com,ls_act_com,all.x = T,all.y = F)
bouquet_diff_flt = bouquet_diff %>% filter(is.na(ENTITY_CODE)) %>% separate(combined,c("STB","cascode"))

write.csv(bouquet_diff_flt, "Bouquets_in_MSR_notin_ListActive.csv",row.names = F)


ls_al_com = list_alacarte %>% select(Customer.Number,Set.Top.Box,Service.CAS.Code,Channel.Name) %>% unite(combined, c("Set.Top.Box","Service.CAS.Code"))

ala_diff = merge(ls_al_com,ls_act_com,all.x = T,all.y = F)
ala_diff_flt = ala_diff %>% filter(is.na(ENTITY_CODE)) %>% separate(combined,c("STB","cascode"))

write.csv(ala_diff_flt, "Alacarte_in_MSR_notin_ListActive.csv",row.names = F)






customer_num = list_bouquet_dated %>% select(Customer.Number,Set.Top.Box) %>% unique() %>% group_by(Customer.Number) %>% summarise(STB.Count = n())
write.csv(customer_num,"Customer numbers with STB count.csv",row.names = F)

#####PLAN COUNT
plan_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=17GoiwT4nWCn0J_7HJF0ZyL5Y0-JPNwOJ&export=download"))
plan_names = plan_names %>% add_row(Plan.Name = c('DD Channels','Bronze basic','Odia FTA'))
ACTV_PLAN = list_active_flt %>% filter(PLAN_NAME %in% plan_names$Plan.Name) %>% select(PLAN_NAME,ENTITY_CODE,ENTITY_NAME,CUSTOMER_NBR,) %>% unique()
ACTV_PLAN_CNT = ACTV_PLAN %>% group_by(PLAN_NAME) %>% summarise(Active.Count = n())
write.csv(ACTV_PLAN_CNT, "PLAN_COUNT_31082022.CSV")
##plan_count with LCO
lco_count = ACTV_PLAN %>% group_by(ENTITY_CODE,ENTITY_NAME,PLAN_NAME) %>% summarise(Active.Count = n())
write.csv(lco_count, "LCOWISE_PLANCOUNT.CSV",row.names = F)
#######cas entitlement

cas_ent = read.csv(file.choose(new = F))
cas_ent$Stb <- gsub("'","",cas_ent$Stb)
cas_ent$Vc <- gsub("'","",cas_ent$Vc)
colnames(cas_ent)[3] <- "VC"
colnames(cas_ent)[4] <- "STB"

ls_act_com = list_active_flt %>% select(CUSTOMER_NBR,VC,SERVICE_CODE,ENTITY_CODE) %>% unite(combined, c("VC","SERVICE_CODE"))
cas_ent_com = cas_ent %>% select(Customer.Nbr,VC,Service.Code,Prov.System.Name) %>% unite(combined, c("VC","Service.Code"))

cas_diff = merge(ls_act_com,cas_ent_com, all.x = T)

not_in_ent_rep = cas_diff %>% filter(is.na(Prov.System.Name)) %>% separate(combined,c('VC','SERVICE_CODE'))
write.csv(not_in_ent_rep, "Not in cas entitle report.csv", row.names = F)



list_export_cond = filter(list_active, ENTITY_CODE == "MDBKT41")
list_export_cond = list_export_cond %>% 
  select(CUSTOMER_NBR,MOBILE_PHONE,PRI_ADDRESS1,BIL_STATE,BIL_CITY) %>%
  distinct()

list_export_cond = list_active %>% 
  select(CUSTOMER_NBR,ENTITY_CODE,ENTITY_NAME,MOBILE_PHONE) %>% 
  distinct() #select only these column and then remove duplicates

active_pivot = list_export_cond %>% group_by(PLAN_NAME) %>%
  summarize(Plan_Count = n()) # make pivot table

# list_bpc = list_active %>% filter(str_detect(ENTITY_CODE, "MSW")) %>%
#   select(CUSTOMER_NBR,STB,MOBILE_PHONE,HOME_PHONE,ENTITY_CODE,ENTITY_NAME) %>% unique()
# write.csv(list_bpc, "Berhampore_customer.CSV", row.names = FALSE)

#head(list_active
#wallet_filtered = filter(wallet, Plan.Details=="Alacarte Plan")
#target = c("Star Jalsha @ 19", "Jalsha Movies @ 6")
#wallet_filter_condition = filter(wallet_filtered, Service.Name %in% target)
#export_condition = wallet_filter_condition %>% select(Customer.Nbr,Customer.Name,Unique.Id,Entity.Code,Entity.Name,Mobile,Service.Name)
write.csv(list_export_cond, "MDBKT41.csv", row.names = FALSE)



##############list active to broadcaster count - run top block first  MSR FORMATT
#remove new plans
new_plan = read.csv(file.choose(new = F))
new_plan_srv = read.csv(file.choose(new = F))
list_active_old = list_active_flt %>% filter(!(PLAN_NAME %in% new_plan$PLAN_NAME))
list_active_new = list_active_flt %>% filter(PLAN_NAME %in% new_plan$PLAN_NAME)
list_active_bc = merge(list_active_old,broadcaster,by.x="SERVICE_CODE",by.y = 'Service.Code',all.x = T,all.y = F)
lsactv_bc_fl = list_active_bc %>% filter(Broadcaster == 'Star India Pvt. Ltd.')
lsactv_old_ala = lsactv_bc_fl[grepl("@",lsactv_bc_fl$SERVICE_NAME),] %>% select(PLAN_NAME,SERVICE_NAME,PRI_STATE,CUSTOMER_NBR,Broadcaster)
lsactv_old_ala_chnname = merge(lsactv_old_ala,service_channel,by.x = 'SERVICE_NAME',by.y = 'Service.Name',all.x = T,all.y = F)
old_ala_final = lsactv_old_ala_chnname %>% select(PLAN_NAME,Channel,PRI_STATE,CUSTOMER_NBR,Broadcaster)
colnames(old_ala_final)[2] <- 'SERVICE_NAME'
lsactv_old_bq = lsactv_bc_fl[!grepl("@",lsactv_bc_fl$SERVICE_NAME),] %>% select(PLAN_NAME,SERVICE_NAME,PRI_STATE,CUSTOMER_NBR,Broadcaster)
lst_pck = rbind(lsactv_old_bq,old_ala_final)

actv_new_serv = merge(list_active_new,new_plan_srv, by.x = 'PLAN_NAME',by.y = 'Plan.Name',all.x = T)
actv_new_serv_clean = actv_new_serv %>% select(PLAN_NAME,Bouquet,PRI_STATE,CUSTOMER_NBR,Broadcaster.Name) %>% unique()
colnames(actv_new_serv_clean)[2] <- 'SERVICE_NAME'
colnames(actv_new_serv_clean)[5] <- 'Broadcaster'
final_data = rbind(actv_new_serv_clean,lst_pck)
LST_PCK_PIVOT = final_data %>% group_by(PRI_STATE,PLAN_NAME,SERVICE_NAME,Broadcaster) %>% summarise(Active.count = n())
write.csv(LST_PCK_PIVOT,"listactive_to_MSR_PLANWISE.CSV",row.names = F)
LST_PCK_PIVOT_SERVICE = final_data %>% group_by(PRI_STATE,SERVICE_NAME,Broadcaster) %>% summarise(Active.count = n())
write.csv(LST_PCK_PIVOT_SERVICE,"listactive_to_MSR.CSV",row.names = F)


######Package work
sports18_ala = list_active %>% filter(PLAN_NAME == 'Alacarte Plan') %>% filter(SERVICE_CODE == 'CH547')
plan_only = list_active %>% filter(PLAN_NAME %in% plan_names$Plan.Name) %>% select(ENTITY_CODE,ENTITY_NAME,CUSTOMER_NBR,CONTRACT_NUMBER,PLAN_CODE,PLAN_NAME) %>% unique()
colnames(plan_only)[6] <- 'BASE_PLAN'
sports18_ala_wbaseplan = merge(sports18_ala,plan_only,all.x = T,all.y = F,by.x = 'CONTRACT_NUMBER',by.y = 'CONTRACT_NUMBER')
write.csv(sports18_ala_wbaseplan,'sports18.csv',row.names = F)
write.csv(plan_only,"planOnly.csv")


#####find new NT2 bouquets and remove
list_active = read.csv(file.choose())
nt2Bouquets = read.csv("C:/Users/Shantanu/Downloads/NEW_BOUQUETS.CSV")
dir_path <- "C:/Users/Shantanu/Downloads"
files <- list.files(dir_path)
pattern <- "_DUEFORRENEWALS\\.CSV$"
matching_files <- files[grep(pattern, files)]
file_info <- file.info(file.path(dir_path, matching_files))
most_recent_file <- matching_files[which.max(file_info$mtime)]
dueRenewal <- read.csv(file.path(dir_path, most_recent_file)) %>% select(Contract.Number,Contract.End.Date)
active_nt2 = merge(list_active,nt2Bouquets,all.x = F)
activeNt2EndDate = merge(active_nt2,dueRenewal,by.x = "CONTRACT_NUMBER", by.y = "Contract.Number") 
activeNt2EndDate$Contract.End.Date = as.Date(activeNt2EndDate$Contract.End.Date, "%d/%m/%Y")
custListSlct1 = activeNt2EndDate %>% filter(Contract.End.Date == today()) %>% select(CONTRACT_NUMBER,SERVICE_CODE) %>% unique()
mqdate = format(today(), format="%d/%m/%Y")
# Loop over each row of the data frame and make an HTTP request for each customer
for (i in 1:nrow(custListSlct1)) {
  # Create the request body for the HTTP request using the customer's account number, mobile number, and type
  body1 <- paste0("<REQUESTINFO>\r\n<CONTRACTINFO>\r\n <CONTRACTNO>",custListSlct1[i, "CONTRACT_NUMBER"],"</CONTRACTNO>\r\n <ORDERDATE>",mqdate,"</ORDERDATE>\r\n <EFFECTIVEDATE>",mqdate,"</EFFECTIVEDATE>\r\n <BILLFREQUENCY></BILLFREQUENCY>\r\n <SALESMANCODE></SALESMANCODE>\r\n <OUTLETS></OUTLETS>\r\n <NOTES></NOTES>\r\n <STATUS></STATUS>\r\n<DELETEINFO>\r\n<PLANCODE>",custListSlct1[i, "SERVICE_CODE"],"</PLANCODE>\r\n<SERVICEGROUPCODE></SERVICEGROUPCODE>\r\n<SERVICECODE></SERVICECODE>\r\n<PACKAGEGROUPCODE></PACKAGEGROUPCODE>\r\n<PACKAGECODE></PACKAGECODE>\r\n</DELETEINFO>\r\n<FLEX-ATTRIBUTE-INFO>\r\n<ATTRIBUTE1></ATTRIBUTE1>\r\n<ATTRIBUTE2></ATTRIBUTE2>\r\n<ATTRIBUTE3></ATTRIBUTE3>\r\n<ATTRIBUTE4></ATTRIBUTE4>\r\n<ATTRIBUTE5></ATTRIBUTE5>\r\n<ATTRIBUTE6></ATTRIBUTE6>\r\n<ATTRIBUTE7></ATTRIBUTE7>\r\n<ATTRIBUTE8></ATTRIBUTE8>\r\n<ATTRIBUTE9></ATTRIBUTE9>\r\n<ATTRIBUTE10></ATTRIBUTE10>\r\n</FLEX-ATTRIBUTE-INFO>\r\n</CONTRACTINFO>\r\n</REQUESTINFO>")
  # Create the headers for the HTTP request
  headers <- c(
    'USERNAME' = 'MB102',
    'PASSWORD' = 'Shan4935',
    'EXTERNALPARTY' = 'MQS',
    'Content-Type' = 'application/xml'
  )
  
  # Generate a random reference number and replace the hardcoded value in the URL with it
  ref_no1 <- paste0(format(runif(1, 510000, 100000000), scientific = FALSE), "a2sbhzd3")
  url1 <- paste0("https://meghbela-bcrm.magnaquest.com/RestService/RestService.svc/ModifyContract?referenceno=", ref_no1)
  
  # Make the HTTP request using the POST method, the request URL, the request body, and the headers
  res <- VERB("POST", url = url1, body = body1, add_headers(headers))
  
  # Print the response to the console
  cat(content(res, 'text'))

  #Sys.sleep(15)
  
}

######Find promotional customers####
plan_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=17GoiwT4nWCn0J_7HJF0ZyL5Y0-JPNwOJ&export=download"))
list_active = read.csv(file.choose())
lcolist = read.csv(file.choose())
list_active_filtered = filter(list_active, PLAN_NAME %in% plan_names$Plan.Name) %>% select(CUSTOMER_NBR,ENTITY_CODE,ENTITY_NAME,LCO_CITY,PLAN_NAME) %>% unique()
lst_active_promo = merge(list_active_filtered,lcolist)
write.csv(lst_active_promo,'promotional_customers.csv')

#########find DPO count####
list_active = read.csv(file.choose())
oplan_dpo = filter(list_active, PLAN_NAME %in% plan_names$Plan.Name) %>% select(CUSTOMER_NBR,PLAN_NAME) %>% unique()
colnames(oplan_dpo)[2] <- "Plan.Name" 
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Diamond_Dig_380 (Promotional)"] <- "DIAMOND DIGITAL @ 380"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "DIAMOND DIGITAL @ 380 (Promotional)"] <- "DIAMOND DIGITAL @ 380"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Gold_353 (Promotional)"] <- "GOLD DIGITAL @ 353"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "GOLD DIGITAL @ 353 (Promotional)"] <- "GOLD DIGITAL @ 353"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Hd_Dha_300(Promotional)"] <- "HD DHAMAKA @ 300"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "HD DHAMAKA @ 300(Promotional)"] <- "HD DHAMAKA @ 300"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Mb_Hd_Beng_455 (Promotional)"] <- "MB BANGLA HD 1 @ 455"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Mb_Hd_Beng_550 (Promotional)"] <- "MB BANGLA HD 2 @ 550"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "MB BANGLA HD 1 @ 455 (Promotional)"] <- "MB BANGLA HD 1 @ 455"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "MB BANGLA HD 2 @ 550 (Promotional)"] <- "MB BANGLA HD 2 @ 550"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Silver_Pow_276 (Promotional)"] <- "Silver Digital Power @ 276"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Silver Digital Power @ 276 (Promotional)"] <- "Silver Digital Power @ 276"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Silver Digital Special @ 276"] <- "Silver Digital Power @ 276"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Meghbela_Bon_330 (Promotional)"] <- "Meghbela Bonanza @ 330"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Meghbela Bonanza @ 330 (Promotional)"] <- "Meghbela Bonanza @ 330"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Meghbela Bonanza Special @ 330"] <- "Meghbela Bonanza @ 330"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Meghbela_Pac_185(Promotional)"] <- "Meghbela Starter Pack @ 185"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Meghbela Starter Pack @ 185 (Promotional)"] <- "Meghbela Starter Pack @ 185"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Meghbela Basic Pack @ 155 (Promo)"] <- "Meghbela Basic Pack @ 155"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Platinum_Dig_450 (Promotional)"] <- "PLATINUM DIGITAL @ 450"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "PLATINUM DIGITAL @ 450 (Promotional)"] <- "PLATINUM DIGITAL @ 450"

oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Diamond_Dig_380"] <- "DIAMOND DIGITAL @ 380"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Gold_Dig_353"] <- "GOLD DIGITAL @ 353"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Hd_Dha_300"] <- "HD DHAMAKA @ 300"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Mb_Hd_Beng_455"] <- "MB BANGLA HD 1 @ 455"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Mb_Hd_Beng_550"] <- "MB BANGLA HD 2 @ 550"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Silver_Pow_276"] <- "Silver Digital Power @ 276"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Meghbela_Bon_330"] <- "Meghbela Bonanza @ 330"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Meghbela_Pac_185"] <- "Meghbela Starter Pack @ 185"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Platinum_Dig_450"] <- "PLATINUM DIGITAL @ 450"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Meghbela_Sta_165"] <- "Meghbela Bengali Starter @165"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Mb_Hd_Hin_455"] <- "MB HINDI HD 1 @ 455"
oplan_dpo$Plan.Name[oplan_dpo$Plan.Name == "Mb_Hd_Hin_550"] <- "MB HINDI HD 2 @ 550"

oplan_pivot = oplan_dpo %>% group_by(Plan.Name) %>% summarize(SubsCount = n())
#####5 report
write.csv(oplan_pivot,"Output/5_DPO_plan_count_Feb23.csv",row.names = F)


####FIND PENETRATION FOR STAR_wb####
bouquet_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=1XvbGWeTDsxEvcvLFH1kFjPFBfSA-lP9K&export=download"))
list_active_wb = list_active %>% filter(LCO_STATE == "WEST BENGAL")
act_cust_count = count(list_active_wb %>% select(CUSTOMER_NBR) %>% unique())
dpo_count = subset(list_active_wb, grepl('^MBIL',list_active_wb$PLAN_CODE,ignore.case = T))
dpo_count = dpo_count %>% group_by(PLAN_NAME) %>% summarise(Active = n())
dpo_count = dpo_count %>% mutate(Penetration = paste0(round(Active/act_cust_count$n*100,2),"%"))
bc_count = list_active_wb %>% filter(PLAN_NAME %in% bouquet_names$Bouquet)
bc_count = bc_count %>% group_by(PLAN_NAME) %>% summarise(Active = n())
bc_count = bc_count %>% mutate(Penetration = paste0(round(Active/act_cust_count$n*100,2),"%"))
write.csv(dpo_count,"dpo_count.csv")
write.csv(bc_count,"bc_count.csv")

####lco dpo pack and customer count####
ls_new_plan = subset(list_active, (grepl('^MBIL',list_active$PLAN_CODE,ignore.case = T)))
plan_pivot = ls_new_plan %>% group_by(ENTITY_CODE,ENTITY_NAME,PLAN_NAME) %>% summarise(DPO_Count = n())
all_pivot = list_active %>% select(ENTITY_CODE,ENTITY_NAME,CUSTOMER_NBR) %>% unique() %>% group_by(ENTITY_CODE,ENTITY_NAME) %>% summarise(Active_customer = n())
all_lco = merge(all_pivot,plan_pivot,all.y = T,all.x = F)
all_lco$DPO_Count[is.na(all_lco$DPO_Count)] <- 0
write.csv(all_lco,"LCO_DPO_count_February24.csv")


###old plans
plan = list_active %>% select(CUSTOMER_NBR,ENTITY_CODE,ENTITY_NAME,PLAN_NAME) %>% unique()
oplan_dpo = filter(plan, PLAN_NAME %in% plan_names$Plan.Name)

oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Diamond_Dig_380 (Promotional)"] <- "DIAMOND DIGITAL @ 380"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "DIAMOND DIGITAL @ 380 (Promotional)"] <- "DIAMOND DIGITAL @ 380"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Gold_353 (Promotional)"] <- "GOLD DIGITAL @ 353"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "GOLD DIGITAL @ 353 (Promotional)"] <- "GOLD DIGITAL @ 353"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Hd_Dha_300(Promotional)"] <- "HD DHAMAKA @ 300"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "HD DHAMAKA @ 300(Promotional)"] <- "HD DHAMAKA @ 300"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Mb_Hd_Beng_455 (Promotional)"] <- "MB BANGLA HD 1 @ 455"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Mb_Hd_Beng_550 (Promotional)"] <- "MB BANGLA HD 2 @ 550"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "MB BANGLA HD 1 @ 455 (Promotional)"] <- "MB BANGLA HD 1 @ 455"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "MB BANGLA HD 2 @ 550 (Promotional)"] <- "MB BANGLA HD 2 @ 550"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Silver_Pow_276 (Promotional)"] <- "Silver Digital Power @ 276"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Silver Digital Power @ 276 (Promotional)"] <- "Silver Digital Power @ 276"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Silver Digital Special @ 276"] <- "Silver Digital Power @ 276"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Meghbela_Bon_330 (Promotional)"] <- "Meghbela Bonanza @ 330"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Meghbela Bonanza @ 330 (Promotional)"] <- "Meghbela Bonanza @ 330"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Meghbela Bonanza Special @ 330"] <- "Meghbela Bonanza @ 330"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Meghbela_Pac_185(Promotional)"] <- "Meghbela Starter Pack @ 185"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Meghbela Starter Pack @ 185 (Promotional)"] <- "Meghbela Starter Pack @ 185"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Meghbela Basic Pack @ 155 (Promo)"] <- "Meghbela Basic Pack @ 155"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Platinum_Dig_450 (Promotional)"] <- "PLATINUM DIGITAL @ 450"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "PLATINUM DIGITAL @ 450 (Promotional)"] <- "PLATINUM DIGITAL @ 450"

oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Diamond_Dig_380"] <- "DIAMOND DIGITAL @ 380"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Gold_Dig_353"] <- "GOLD DIGITAL @ 353"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Hd_Dha_300"] <- "HD DHAMAKA @ 300"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Mb_Hd_Beng_455"] <- "MB BANGLA HD 1 @ 455"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Mb_Hd_Beng_550"] <- "MB BANGLA HD 2 @ 550"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Silver_Pow_276"] <- "Silver Digital Power @ 276"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Meghbela_Bon_330"] <- "Meghbela Bonanza @ 330"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Meghbela_Pac_185"] <- "Meghbela Starter Pack @ 185"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Platinum_Dig_450"] <- "PLATINUM DIGITAL @ 450"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Meghbela_Sta_165"] <- "Meghbela Bengali Starter @165"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Mb_Hd_Hin_455"] <- "MB HINDI HD 1 @ 455"
oplan_dpo$PLAN_NAME[oplan_dpo$PLAN_NAME == "Mb_Hd_Hin_550"] <- "MB HINDI HD 2 @ 550"

plan_pivot = oplan_dpo %>% group_by(ENTITY_CODE,ENTITY_NAME,PLAN_NAME) %>% summarize(DPO_count = n())

####295 pack Compare count ####
ls_act_1st_month = read.csv(file.choose()) #OLD MONTH
ls_act_2nd_month = read.csv(file.choose()) #NEW MONTH
base_plan = read.csv(sprintf("https://drive.google.com/u/0/uc?id=17GoiwT4nWCn0J_7HJF0ZyL5Y0-JPNwOJ&export=download"))
base_plan = base_plan %>% add_row(Plan.Name = c('DD Channels','Bronze basic','Odia FTA'))

royal_act_2nd = ls_act_2nd_month %>% filter(PLAN_CODE == "MBILROYAL" ) %>% select(CUSTOMER_NBR,STB,SC,PLAN_CODE,PLAN_NAME,ENTITY_CODE,ENTITY_NAME,LCO_CITY)
base_plan_1st = ls_act_1st_month %>% filter(PLAN_NAME %in% base_plan$Plan.Name) %>% select(SC,PLAN_CODE,PLAN_NAME)
royal_merge = merge(royal_act_2nd,base_plan_1st,by.x = "SC",by.y = "SC",all.x = T,all.y = F)
royal_merge$PLAN_NAME.y[royal_merge$PLAN_NAME.y == "Bronze basic"] <- "Bronze basic @ 155"
royal_merge$aggregator <- as.numeric(gsub(".* @ (\\d+)$", "\\1", royal_merge$PLAN_NAME.y))
royal_merge$aggregator[is.na(royal_merge$aggregator)] <- 0
royal_merge = royal_merge %>% mutate(Status = ifelse(aggregator > 295, 'Downgraded','Upgraded/Same'), .after = NULL)
royal_pivot = royal_merge %>% group_by(LCO_CITY,ENTITY_CODE,ENTITY_NAME,Status) %>% summarise(Count = n())
royal_pivot <- royal_pivot[order(royal_pivot$ENTITY_CODE),]
write.csv(royal_pivot, "295_pack_status_February24.csv",row.names = F)


####Find channelwise package count areawise ####
lsactv = read.csv(file.choose())
packchn = read.csv(file.choose()) ## create a planwise channel file from channelwsie count report

actvAla = lsactv %>% filter(PLAN_NAME == "Alacarte Plan") %>% filter(SERVICE_CODE %in% c("CH1")) %>%
  select(CUSTOMER_NBR,ENTITY_CODE,LCO_CITY,PLAN_NAME,SERVICE_NAME) %>% unique()
actvOther = lsactv %>% filter(!(PLAN_NAME == "Alacarte Plan")) %>% select(CUSTOMER_NBR,ENTITY_CODE,LCO_CITY,PLAN_NAME) %>% unique()
actvOthChnl = merge(actvOther,packchn)

actvAla$SERVICE_NAME[actvAla$SERVICE_NAME == "Zee Tv @ 19"] <- "Zee TV"
#actvAla$SERVICE_NAME[actvAla$SERVICE_NAME == "Zee Bangla @ 19"] <- "Zee Bangla"
#actvAla$SERVICE_NAME[actvAla$SERVICE_NAME == "Zee Bangla Cinema @ 10"] <- "Zee Bangla Cinema"
actvAla = actvAla %>% mutate(Type = "AlcartePlan")
colnames(actvAla)[5] <- "Channel.Name"
actvAla = actvAla %>% relocate(PLAN_NAME)
actvAll = rbind(actvOthChnl,actvAla)
write.csv(actvAll,"ChannelWise_.csv",row.names = F)
