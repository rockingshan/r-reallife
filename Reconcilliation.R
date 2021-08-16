library(tidyverse)
library(dplyr)
library(readxl)
library(purrr)

##opens a window to select files, 
list_active = read.csv(choose.files(default = "_LISTOFACTIVE.CSV",caption = "Select Active Customer File",multi = FALSE,), skip = 1, header = FALSE, colClasses = c("character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL") ) #import MQ data
colnames(list_active) <- c("CUSTOMER_NBR","CONTRACT_NUMBER","ENTITY_CODE","ENTITY_NAME","LCO_CITY","LCO_STATE","FIRST_NAME","MIDDLE_NAME","LAST_NAME","STB","SC","SERVICE_NAME","SERVICE_CODE","CASCODE","PLAN_CODE","PLAN_NAME","BILLING_FREQUENCY","MOBILE_PHONE","EMAIL","HOME_PHONE","PRI_STATE","PRI_CITY","PRI_ADDRESS1")
###inventory
inventory = read.csv(choose.files(default = "_INVENTORY.CSV",caption = "Select Inventory File",multi = FALSE,),colClasses = c(SERIAL_NUMBER="character"))
inventory_select = select(inventory, SERIAL_NUMBER,ENTITY_CODE)



#import safeview data and searialze
list_sfw = read.csv(choose.files(default = "",caption = "Select Safeview CAS File",multi = FALSE,),colClasses = c(SubscriptionID="character")) #specify particular column as character
sfw_cas_data = list_sfw %>% unite(combined, c("SMCs","SubscriptionID"))

#import gospell data 
#### Gospell CAS file does not have column names. Please add column names, adding column names separately
list_gospell = read.csv(choose.files(default = "",caption = "Select Gospell CAS File",multi = FALSE,), header = FALSE)
colnames(list_gospell) <- c("vc", "cascode")
GSPL_cas_data = list_gospell %>% unite(combined, c("vc", "cascode"))

##import ABV boxes
sheets <- excel_sheets("CASEntitlement.xlsx")
data_sheets <- sheets[grepl("CASEntitlement", sheets)]
sheet_df <- map_dfr(data_sheets, ~read_excel("CASEntitlement.xlsx", sheet = .x, skip = 1), id = .x)
abv_cas_data = filter(sheet_df, STATUS == "Activated")
#abv_cas_data = read_xlsx(choose.files(default = "",caption = "Select ABV CAS File",multi = FALSE,))
abv_cas_data_combn = abv_cas_data %>% unite(combined, c("SMARTCARDNO","PACKAGEID"))

##following block must be run before running any cas block. This prepares the MQ data
## replace ' in column data, change to proper column names
list_active$STB <- gsub("'","",list_active$STB)
list_active$SC <- gsub("'","",list_active$SC)
colnames(list_active)[10] <- "VC"
colnames(list_active)[11] <- "STB"
list_active <- list_active %>% mutate(VC.length = nchar(VC),  .after = 11) # get character length of vc
list_active$VC.length <- gsub("8","GOSPELL",list_active$VC.length, fixed = TRUE)
list_active$VC.length <- gsub("12","SAFEVIEW",list_active$VC.length, fixed = TRUE)
list_active$VC.length <- gsub("16","ABV",list_active$VC.length, fixed = TRUE) #REPLACE LENGTHS TO CAS NAMES
#CREATE SEPERATE DATA FOR CAS
list_ac_SFW = filter(list_active, VC.length == "SAFEVIEW")
list_ac_GSPL = filter(list_active, VC.length == "GOSPELL")
list_ac_ABV = filter(list_active, VC.length == "ABV")

#SAFEVIEW OPERATION on MQ data
list_ac_SFW = separate(list_ac_SFW, VC, into = c("leftval", "rightval"), sep = 10, remove = FALSE)
list_ac_SFW$leftval = as.numeric(list_ac_SFW$leftval)
list_ac_SFW = list_ac_SFW %>% unite(combined, c("leftval","CASCODE"))  
mq_sfw_data = list_ac_SFW %>% select(combined,CUSTOMER_NBR,VC,STB,SERVICE_NAME) %>% distinct()
reconcile_data_SFW = merge(x = sfw_cas_data, y = mq_sfw_data, by = "combined", all.x = TRUE) # vlookup cas data with mq data, cas data on left side
recon_sfw_NA_output = reconcile_data_SFW %>% filter(is.na(CUSTOMER_NBR))
recon_sfw_NA_output = separate(recon_sfw_NA_output, combined, c("vc","casocde"))
write.csv(recon_sfw_NA_output, "Safeview_active_service_not_in_MQ.csv", row.names = F)

#GOSPELL operation GOSPELL
list_ac_GSPL = list_ac_GSPL %>% unite(combined, c("VC","CASCODE"))
mq_GSPL_data = list_ac_GSPL %>% select(combined,CUSTOMER_NBR,STB,SERVICE_NAME) %>% distinct()
reconcile_data_GSPL = merge(x = GSPL_cas_data, y = mq_GSPL_data, by = "combined", all.x = TRUE) # vlookup cas data with mq data, cas data on left side
recon_GSPL_NA_output = reconcile_data_GSPL %>% filter(is.na(CUSTOMER_NBR))
recon_GSPL_NA_output = separate(recon_GSPL_NA_output, combined, c("vc","cascode"))
recon_GSPL_NA_output = select(recon_GSPL_NA_output, vc,cascode)
write.csv(recon_GSPL_NA_output, "Gospell_active_service_not_in_MQ.csv", row.names = F)


#####ABV operation
list_ac_ABV = list_ac_ABV %>% unite(combined, c("VC","CASCODE"))
mq_ABV_data = list_ac_ABV %>% select(combined,CUSTOMER_NBR,STB,SERVICE_NAME) %>% distinct()
reconcile_data_ABV = merge(x = abv_cas_data_combn, y = mq_ABV_data, by = "combined",all.x = TRUE)
recon_ABV_NA_output = reconcile_data_ABV %>% filter(is.na(CUSTOMER_NBR))
recon_ABV_NA_output = separate(recon_ABV_NA_output, combined, c("vc","cascode"))
write.csv(recon_ABV_NA_output, "ABV_active_service_not_in_MQ.csv", row.names = F)



# list_active_bpc_ABV = list_ac_ABV %>% filter(str_detect(ENTITY_CODE, "MSW")) %>% select(CUSTOMER_NBR,CONTRACT_NUMBER,ENTITY_CODE,ENTITY_NAME,VC,STB,MOBILE_PHONE) %>% unique()
# write.csv(list_active_bpc_ABV,"bpc_ABV.csv",row.names = F)
# list_active_bpc_GSPL_1 = list_active %>% filter(str_detect(ENTITY_CODE, "MDSKWJV")) %>% select(CUSTOMER_NBR,CONTRACT_NUMBER,ENTITY_CODE,ENTITY_NAME,VC,STB,MOBILE_PHONE) %>% unique()

# stb_count = select(list_active,CUSTOMER_NBR,CONTRACT_NUMBER,ENTITY_CODE,ENTITY_NAME,VC,STB,MOBILE_PHONE) %>% unique()
# stb_count_pivot = stb_count %>% group_by(ENTITY_CODE) %>% summarize(Active_cust = n())
# write.csv(stb_count_pivot, "COUNT.csv", row.names = F)
# write.csv(stb_count, "details.csv", row.names = F)

customer_data = list_active %>% select(CUSTOMER_NBR,ENTITY_CODE,LCO_CITY,LCO_STATE,FIRST_NAME,MOBILE_PHONE,PRI_STATE,PRI_CITY,PRI_ADDRESS1) %>% unique()
write.csv(customer_data,"Customer.csv",row.names = F)
