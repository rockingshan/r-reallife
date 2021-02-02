library(tidyverse)
library(here)
library(dplyr)
library(gapminder) 
list_active = read.csv(here("data", "4319101_LISTOFACTCUST.CSV"))

ent_t = list_active %>% select(ENTITY_CODE) %>% distinct()
write.csv(ent_t, "1.csv")
#import safeview data and searialze
list_sfw = read.csv(here("data","safeview.CSV"),colClasses = c(SubscriptionID="character")) #specify particular column as character
sfw_cas_data = list_sfw %>% unite(combined, c("SMCs","SubscriptionID"))

#import gospell data
list_gospell = read.csv(here("data", "merge-csv.com__6017eff1db5c2.csv"))
GSPL_cas_data = list_gospell %>% unite(combined, c("VC", "Product.ID"))

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

#GOSPELL operation
list_ac_GSPL = list_ac_GSPL %>% unite(combined, c("VC","CASCODE"))
mq_GSPL_data = list_ac_GSPL %>% select(combined,CUSTOMER_NBR,STB,SERVICE_NAME) %>% distinct()
reconcile_data_GSPL = merge(x = GSPL_cas_data, y = mq_GSPL_data, by = "combined", all.x = TRUE) # vlookup cas data with mq data, cas data on left side
recon_GSPL_NA_output = reconcile_data_GSPL %>% filter(is.na(CUSTOMER_NBR))
recon_GSPL_NA_output = separate(recon_GSPL_NA_output, combined, c("vc","casocde"))
write.csv(recon_GSPL_NA_output, "Gospell_active_service_not_in_MQ.csv", row.names = F)