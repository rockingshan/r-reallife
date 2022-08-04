library(tidyverse)
library(here)
library(dplyr)
library(gapminder) 
library(readxl)

##opens a window to select files, 
list_active = read.csv(choose.files(default = "_LISTOFACTIVE.CSV",caption = "Select Active Customer File",multi = FALSE,), skip = 1, header = FALSE, colClasses = c("character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL") ) #import MQ data
colnames(list_active) <- c("CUSTOMER_NBR","CONTRACT_NUMBER","ENTITY_CODE","ENTITY_NAME","LCO_CITY","LCO_STATE","FIRST_NAME","MIDDLE_NAME","LAST_NAME","STB","SC","SERVICE_NAME","SERVICE_CODE","CASCODE","PLAN_CODE","PLAN_NAME","BILLING_FREQUENCY","MOBILE_PHONE")
###inventory
inventory = read.csv(choose.files(default = "_INVENTORY.CSV",caption = "Select Inventory File",multi = FALSE,),colClasses = c(SERIAL_NUMBER="character"))
inventory_select = select(inventory, SERIAL_NUMBER,ENTITY_CODE)


#import gospell data
list_gospell = read.csv(choose.files(default = "",caption = "Select Gospell CAS File",multi = FALSE,), header = FALSE)
colnames(list_gospell) <- c("vc", "cascode")
GSPL_cas_data = list_gospell %>% unite(combined, c("vc", "cascode"))

# ### area wise function
# areawise_data_export <- function(gospell){
#   ## filter area code and remove duplicate then convert to a list for loop
#   area_list = gospell %>% select(CITY) %>% distinct()
#   area_list = area_list[['CITY']]
#   
#   ## run the loop according to the list and export csv for each LCO
#   for (areacode in area_list) {
#     gospell_filtered = filter(gospell, CITY==areacode)
#     write.csv(gospell_filtered, paste(areacode,".csv", sep = ""), row.names = FALSE)
#   }
# }

## replace ' in column data, change to proper column names
list_active$STB <- gsub("'","",list_active$STB)
list_active$SC <- gsub("'","",list_active$SC)
colnames(list_active)[10] <- "VC"
colnames(list_active)[11] <- "STB"
list_active <- list_active %>% mutate(VC.length = nchar(VC),  .after = 11) # get character length of vc
list_active$VC.length <- gsub("8","GOSPELL",list_active$VC.length, fixed = TRUE)


#CREATE SEPERATE DATA FOR CAS
list_ac_GSPL = filter(list_active, VC.length == "GOSPELL")

#GOSPELL operation GOSPELL
gospell_ac_vc = select(list_ac_GSPL, VC,CUSTOMER_NBR) %>% unique()
write.csv(gospell_ac_vc, "gospell_active.csv", row.names = F)

list_ac_GSPL = list_ac_GSPL %>% unite(combined, c("VC","CASCODE"))
mq_GSPL_data = list_ac_GSPL %>% select(combined,CUSTOMER_NBR,STB,SERVICE_NAME) %>% distinct()
reconcile_data_GSPL = merge(x = GSPL_cas_data, y = mq_GSPL_data, by = "combined", all.x = TRUE) # vlookup cas data with mq data, cas data on left side
recon_GSPL_NA_output = reconcile_data_GSPL %>% filter(is.na(CUSTOMER_NBR))
recon_GSPL_NA_output = separate(recon_GSPL_NA_output, combined, c("vc","cascode"))
recon_GSPL_NA_output = select(recon_GSPL_NA_output, vc,cascode)
write.csv(recon_GSPL_NA_output, "Gospell_active_service_not_in_MQ.csv", row.names = F)

gospell_log <- read.csv(here("data/all.txt"))# import all disconnections last few months
gospell_log_sh <- filter(gospell_log, X2020.07.10.00.00.00 == "2030-12-31 00:00:00") %>%
  select(X67728364,X950) %>% unique()
colnames(gospell_log_sh)[1] <- "VC"
gospell_act = read_csv("gospell_active.csv")
gspl <- left_join(gospell_log_sh,gospell_act ,all.x = T ) %>% filter(is.na(CUSTOMER_NBR))
gspl <- select(gspl, VC,X950)
colnames(gspl)[1] <- "vc"
colnames(gspl)[2] <- "cascode"
GSPL_all <- rbind(recon_GSPL_NA_output,gspl) %>% unique()
colnames(GSPL_all)[2] <- "cascode"
colnames(GSPL_all)[1] <- "SERIAL_NUMBER"
GSPL_all_AREA = left_join(GSPL_all,inventory_select,all.x = T)

colnames(recon_GSPL_NA_output)[1] <- "SERIAL_NUMBER"
GSPL_all_AREA = left_join(recon_GSPL_NA_output,inventory_select,all.x = T) ## without importing disconnection commands, straight compare

write.csv(GSPL_all_AREA, "Output/GOSPELL INACTIVE COMMANDS RECEND.CSV", row.names = F)

gospell_all_citywise = read.csv("GOSPELL INACTIVE COMMANDS RECEND.CSV")
areawise_data_export(gospell_all_citywise)


################## gospell already resend command
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
gspl_combine = merge(GSPL_com_log,mq_GSPL_data, all.x = T,all.y = F) ###for run after some time. for laready generated run below line
#gspl_combine = merge(pro_log,mq_GSPL_data, all.x = T,all.y = F)
recon_GSPL_NA_output = gspl_combine %>% filter(is.na(CUSTOMER_NBR))
recon_GSPL_NA_output = separate(recon_GSPL_NA_output, combined, c("vc","cascode"))
recon_GSPL_NA_output = select(recon_GSPL_NA_output, vc,cascode)
recon_GSPL_NA_output_ent = merge(recon_GSPL_NA_output,inventory_select, all.x = T)

write.csv(recon_GSPL_NA_output_ent, "Output/Gospell_command log.csv", row.names = F)

#####
gspl_na = read_csv(file.choose(new = F))
gspl_na_fl = gspl_na %>% filter(ENTITY_CODE == "MDCH161")
write.csv(gspl_na_fl, "Output/Gospell_command_LCOWISE_MDch161.csv", row.names = F)