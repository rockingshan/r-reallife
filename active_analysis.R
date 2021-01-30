library(tidyverse)
library(here)
library(dplyr)
library(gapminder) 
<<<<<<< HEAD
list_active = read.csv(here("data/4050632_LISTOFACTCUST.CSV"))
=======
list_active = read.csv(here("", "3990773_LISTOFACTCUST.CSV"))
>>>>>>> 13565ac7750583fccd4d94b10d4e1c1fe0fd63db
## replace ' in column data, change to proper column names

list_active$STB <- gsub("'","",list_active$STB)
list_active$SC <- gsub("'","",list_active$SC)
colnames(list_active)[10] <- "VC"
colnames(list_active)[11] <- "STB"
list_active <- list_active %>% mutate(VC.length = nchar(VC),  .after = 11) # get character length of vc
list_active$VC.length <- gsub("8","GOSPELL",list_active$VC.length, fixed = TRUE)
list_active$VC.length <- gsub("12","SAFEVIEW",list_active$VC.length, fixed = TRUE)
list_active$VC.length <- gsub("16","ABV",list_active$VC.length, fixed = TRUE) #REPLACE LENGTHS TO CAS NAMES

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