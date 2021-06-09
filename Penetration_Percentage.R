library(tidyverse)
library(dplyr)
library(readxl)

##opens a window to select files, 
list_active = read.csv(choose.files(default = "_LISTOFACTIVE.CSV",caption = "Select Active Customer File",multi = FALSE,), skip = 1, header = FALSE, colClasses = c("character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL") ) #import MQ data
colnames(list_active) <- c("CUSTOMER_NBR","CONTRACT_NUMBER","ENTITY_CODE","ENTITY_NAME","LCO_CITY","LCO_STATE","FIRST_NAME","MIDDLE_NAME","LAST_NAME","STB","SC","SERVICE_NAME","SERVICE_CODE","CASCODE","PLAN_CODE","PLAN_NAME","BILLING_FREQUENCY","MOBILE_PHONE")
plan_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=1WpHeGDD6syAEuEwrSil8CIewbjZweLHb&export=download"))
bouquet_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=1iHErLr_cL36BWzYwsQjOId-YlWWWbAr1&export=download"))
direct_customer = c(WBTDCL,MBDSMLHT,MBDRLNDHT,MDKH,MDCNDP,MDBQA,MDDHH,MDSKWJV)
list_active$STB <- gsub("'","",list_active$STB)
list_active$SC <- gsub("'","",list_active$SC)
colnames(list_active)[10] <- "VC"
colnames(list_active)[11] <- "STB"

lsa_kol = list_active %>% filter(str_detect(ENTITY_CODE, "MD0")) %>% add_column(Area = "Kolkata",.after = 4)
lsa_cndp = list_active %>% filter(str_detect(ENTITY_CODE, "MDCH")) %>% add_column(Area = "Chandipur",.after = 4)
lsa_hcs = list_active %>% filter(str_detect(ENTITY_CODE, "HCS")) %>% add_column(Area = "Haldia",.after = 4)
lsa_msw = list_active %>% filter(str_detect(ENTITY_CODE, "MSW")) %>% add_column(Area = "Berhampore",.after = 4)
lsa_bqa = list_active %>% filter(str_detect(ENTITY_CODE, "MBK")) %>% add_column(Area = "Bankura",.after = 4)
lsa_bkt = list_active %>% filter(str_detect(ENTITY_CODE, "MDBKT")) %>% add_column(Area = "Bakrahat",.after = 4)
lsa_dh1 = list_active %>% filter(str_detect(ENTITY_CODE, "MBDH")) %>% add_column(Area = "Dinhata",.after = 4)
lsa_dh2 = list_active %>% filter(str_detect(ENTITY_CODE, "MDH")) %>% add_column(Area = "Dinhata",.after = 4)
lsa_odr = list_active %>% filter(str_detect(ENTITY_CODE, "MDOR")) %>% add_column(Area = "Odisha",.after = 4)
lsa_direct = list_active %>% filter(ENTITY_CODE %in% c("WBTDCL","MBDSMLHT","MBDRLNDHT","MDKH","MDCNDP","MDBQA","MDDHH","MDSKWJV","MBDML","TESTENTITY")) %>% add_column(Area = "Direct",.after = 4)
list_active_area = rbind(lsa_bkt,lsa_bqa,lsa_cndp,lsa_dh1,lsa_dh2,lsa_direct,lsa_hcs,lsa_kol,lsa_msw,lsa_odr)
remove(list_active,lsa_bkt,lsa_bqa,lsa_cndp,lsa_dh1,lsa_dh2,lsa_direct,lsa_hcs,lsa_kol,lsa_msw,lsa_odr)
