library(readr)
library(tidyverse)
library(dplyr)

cust_historical_start <- read.csv(file.choose(new = F),colClasses = c("character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character"))
cust_historical_start <- cust_historical_start %>% mutate(VC.length = nchar(Smartcard.Serialnumber),  .after = 4)
cust_historical_start$VC.length <- gsub("8","GOSPELL",cust_historical_start$VC.length, fixed = TRUE)
cust_historical_start$VC.length <- gsub("12","SAFEVIEW",cust_historical_start$VC.length, fixed = TRUE)
cust_historical_start$VC.length <- gsub("16","ABV",cust_historical_start$VC.length, fixed = TRUE) #REPLACE LENGTHS TO CAS NAMES
###############
cust_historical_end <- read.csv(file.choose(new = F),colClasses = c("character","character","NULL","character","character","NULL","NULL","character","character","NULL","character","character","character","character","character","character","character","NULL"))
cust_historical_end <- cust_historical_end %>% mutate(VC.length = nchar(Smartcard.Serialnumber),  .after = 4)
cust_historical_end$VC.length <- gsub("8","GOSPELL",cust_historical_end$VC.length, fixed = TRUE)
cust_historical_end$VC.length <- gsub("12","SAFEVIEW",cust_historical_end$VC.length, fixed = TRUE)
cust_historical_end$VC.length <- gsub("16","ABV",cust_historical_end$VC.length, fixed = TRUE) #REPLACE LENGTHS TO CAS NAMES
###############
# cust_hist_trim = select(cust_historical,Customer.Nbr,Set.Box.Number,Smartcard.Serialnumber,Name,Mobile) %>% unique()
# custhist_count = select(cust_hist_trim,Customer.Nbr) %>% unique()
# cust_historical <- cust_historical %>% mutate(VC.length = nchar(Smartcard.Serialnumber),  .after = 11)
# cust_historical$VC.length <- gsub("16","ABV",cust_historical$VC.length, fixed = TRUE)
# list_ac_ABV = filter(cust_historical, VC.length == "ABV") %>% select(Smartcard.Serialnumber,Package.Code)
# list_ac_ABV_codes = merge(list_ac_ABV,cas_code,all.x = TRUE) %>% na.omit(list_ac_ABV_codes, cols='Provision.Code')
# #VC_count = list_ac_ABV %>% select(Smartcard.Serialnumber) %>% unique()
# write.csv(cust_hist_trim,"Customer_backdate.csv", row.names = F)

#########################
#find customer decreased in this period
cust_start = cust_historical_start %>% select(Customer.Nbr,Set.Box.Number,STB.Item.Descr,Smartcard.Serialnumber,VC.length) %>% unique()
cust_end = cust_historical_end %>% select(Customer.Nbr,Set.Box.Number,STB.Item.Descr,Smartcard.Serialnumber,VC.length) %>% unique()
cust_drop = merge(cust_start,cust_end, by.x = 'Set.Box.Number', by.y = 'Set.Box.Number', all.x = T, all.y = F)
cust_drop_det = cust_drop[is.na(cust_drop$Customer.Nbr.y),]
cust_drop_piv = cust_drop_det %>% group_by(STB.Item.Descr.x) %>% summarise(Type.Count = n())
write.csv(cust_drop_piv,"STBMODEL_DEACTIVATED_YEARONYEAR.csv",row.names = F)
cust_drop_piv = cust_drop_det %>% group_by(VC.length.x) %>% summarise(Type.Count = n())
write.csv(cust_drop_piv,"CASL_DEACTIVATED_YEARONYEAR.csv",row.names = F)



###for schedule ix give data to abv####
cust_historical <- read.csv(file_name<-file.choose(new = F),colClasses = c("character","character","NULL","character","character","NULL","NULL","character","character","NULL","character","character","character","character","character","character","character","NULL"))
file_name <- basename(file_name)
date_part <- regmatches(file_name, regexpr("\\d{8}", file_name))
cust_historical <- cust_historical %>% mutate(VC.length = nchar(Smartcard.Serialnumber),  .after = 4)
cust_historical$VC.length <- gsub("8","GOSPELL",cust_historical$VC.length, fixed = TRUE)
cust_historical$VC.length <- gsub("12","SAFEVIEW",cust_historical$VC.length, fixed = TRUE)
cust_historical$VC.length <- gsub("16","ABV",cust_historical$VC.length, fixed = TRUE) #REPLACE LENGTHS TO CAS NAMES
cust_historical_abv <- cust_historical %>% filter(VC.length == "ABV") %>% select(Smartcard.Serialnumber,Set.Box.Number,Package.Code,Package.Descr,Activation.date)
cascode = read.csv(file.choose(new = F))
cascode_abv = cascode %>% filter(Prov.Sys.Name == "ABV") %>% filter(Service.Status == "Active") %>% filter(Provsion.Status == "A")
cascode_abv$Provsion.Code <-  gsub("'","",cascode_abv$Provsion.Code)
colnames(cascode_abv)[1] <- "Package.Code"
cust_hist_abv_code = merge(cust_historical_abv,cascode_abv,all.x = T,all.y = F)
##cust_hist_abv_code = na.omit(cust_hist_abv_code)
cust_hist_abv_code1  = cust_hist_abv_code %>% filter(!(Package.Descr == "Ncf  Slab")) %>% select(Smartcard.Serialnumber,Set.Box.Number,Provsion.Code,Activation.date)
colnames(cust_hist_abv_code1)[4] <- "Package.Start.Date"
output_file_name <- paste0("MEGB_PACKAGEWISEDATA_", date_part, ".csv")
write.csv(cust_hist_abv_code1,output_file_name,row.names = F)


###with stock####
inventory = read.csv(file.choose(new = F),colClasses = c(SERIAL_NUMBER="character")) ##inventory file
lco_mas = read.csv(file.choose(new = F)) #lco master
lco_mas$Lco.Code <- gsub("'","",lco_mas$Lco.Code)
lco_mas <- lco_mas %>% select(Lco.Code,Business.Name,City)

act_cust = cust_historical_start %>% select(Customer.Nbr) %>% unique()
act_cust = act_cust %>% mutate(Status = "Active")
inv_cus = inventory %>% filter(TYPE == "Customer")
inv_cus_stat = merge(inv_cus,act_cust,by.x = "CUSTOMER_NBR",by.y = "Customer.Nbr", all.x = T)
inv_cus_stat$Status[is.na(inv_cus_stat$Status)] <- 'Inactive'
inv_ent = inventory %>% filter(TYPE == "Entity")
inv_ent = inv_ent %>% mutate(Status = "In LCO Store")
inv_all = rbind(inv_cus_stat,inv_ent)
##check
cardtype = c('Smart Card','ABV Smart Card','Sumavision SC','Gospell SC','Safeview SC','Nagra Cardless STB')
inv_ts = inv_all %>% filter(Status == "Active") %>% filter(ITEM_DESCR %in% cardtype)

# inv_cust_pivot = inv_all %>% group_by(ENTITY_CODE,Status) %>% summarise(STB.Count = n()) %>%
#   pivot_wider(names_from = Status,values_from = STB.Count)
# inv_cust_pivot[is.na(inv_cust_pivot)] <- 0
inv_cust__d = merge(inv_all,lco_mas, by.x = 'ENTITY_CODE',by.y = 'Lco.Code')
write.csv(inv_cust__d,"Output/Inventory_customer_31032023.csv",row.names = F)


####abv_data####
#card_data = 
####cas data cehck####
dz = read.csv(file.choose())  #service code import
cust_historical_start <- read.csv(file.choose(new = F),colClasses = c("character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character"))
cust_historical_start <- cust_historical_start %>% mutate(VC.length = nchar(Smartcard.Serialnumber),  .after = 4)
cust_historical_start$VC.length <- gsub("8","GOSPELL",cust_historical_start$VC.length, fixed = TRUE)
cust_historical_start$VC.length <- gsub("12","SAFEVIEW",cust_historical_start$VC.length, fixed = TRUE)
cust_historical_start$VC.length <- gsub("16","ABV",cust_historical_start$VC.length, fixed = TRUE) #REPLACE LENGTHS TO CAS NAMES
df = cust_historical_start %>% filter(VC.length == "ABV")

da = merge(df,dz)
df1 = da %>% select(Customer.Nbr,Smartcard.Serialnumber,Provsion.Code,Activation.date)
#df1 = da %>% group_by(Provsion.Code) %>% summarise(Count = n())
write.csv(df1,"Packagewsie_Details_31122021.csv",row.names = F)
