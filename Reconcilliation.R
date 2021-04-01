library(tidyverse)
library(here)
library(dplyr)
library(gapminder) 
library(readxl)

list_active = read.csv(here("data", "4593409_LISTOFACTCUST.CSV"))  #import MQ data
###inventory
inventory = read.csv(here("data","4519064_INVENTDATA.CSV"),colClasses = c(SERIAL_NUMBER="character"))
inventory_select = select(inventory, SERIAL_NUMBER,ENTITY_CODE)

# ent_t = list_active %>% select(ENTITY_CODE) %>% distinct()
# write.csv(ent_t, "1.csv")
#import safeview data and searialze
list_sfw = read.csv(here("data","safeview.CSV"),colClasses = c(SubscriptionID="character")) #specify particular column as character
sfw_cas_data = list_sfw %>% unite(combined, c("SMCs","SubscriptionID"))

#import gospell data
list_gospell = read.csv(here("data", "gospell_all.csv")) #import old and current gospell combine data
GSPL_cas_data = list_gospell %>% unite(combined, c("vc", "cascode"))

##import ABV boxes
abv_cas_data = read_xlsx(here("data/ABV.xlsx"))
abv_cas_data_combn = abv_cas_data %>% unite(combined, c("SMARTCARDNO","PACKAGEID"))

### area wise function
areawise_data_export <- function(gospell){
  ## filter area code and remove duplicate then convert to a list for loop
  area_list = gospell %>% select(CITY) %>% distinct()
  area_list = area_list[['CITY']]
  
  ## run the loop according to the list and export csv for each LCO
  for (areacode in area_list) {
    gospell_filtered = filter(gospell, CITY==areacode)
    write.csv(gospell_filtered, paste(areacode,".csv", sep = ""), row.names = FALSE)
  }
}

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
gospell_ac_vc = select(list_ac_GSPL, VC,CUSTOMER_NBR) %>% unique()
write.csv(gospell_ac_vc, "gospell_active.csv", row.names = F)

list_ac_GSPL = list_ac_GSPL %>% unite(combined, c("VC","CASCODE"))
mq_GSPL_data = list_ac_GSPL %>% select(combined,CUSTOMER_NBR,STB,SERVICE_NAME) %>% distinct()
reconcile_data_GSPL = merge(x = GSPL_cas_data, y = mq_GSPL_data, by = "combined", all.x = TRUE) # vlookup cas data with mq data, cas data on left side
recon_GSPL_NA_output = reconcile_data_GSPL %>% filter(is.na(CUSTOMER_NBR))
recon_GSPL_NA_output = separate(recon_GSPL_NA_output, combined, c("vc","cascode"))
recon_GSPL_NA_output = select(recon_GSPL_NA_output, vc,cascode)
write.csv(recon_GSPL_NA_output, "Gospell_active_service_not_in_MQ.csv", row.names = F)

gospell_log <- read.csv(here("F:/#reconcile_may_2018/all.txt"))# import all disconnections last few months
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
write.csv(GSPL_all_AREA, "GOSPELL INACTIVE COMMANDS RECEND.CSV", row.names = F)

gospell_all_citywise = read.csv("GOSPELL INACTIVE COMMANDS RECEND.CSV")
areawise_data_export(gospell_all_citywise)

#####ABV operation
list_ac_ABV = list_ac_ABV %>% unite(combined, c("VC","CASCODE"))
mq_ABV_data = list_ac_ABV %>% select(combined,CUSTOMER_NBR,STB,SERVICE_NAME) %>% distinct()
reconcile_data_ABV = merge(x = abv_cas_data_combn, y = mq_ABV_data, by = "combined",all.x = TRUE)
recon_ABV_NA_output = reconcile_data_ABV %>% filter(is.na(CUSTOMER_NBR))
recon_ABV_NA_output = separate(recon_ABV_NA_output, combined, c("vc","cascode"))
write.csv(recon_ABV_NA_output, "ABV_active_service_not_in_MQ.csv", row.names = F)
