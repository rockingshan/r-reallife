library(tidyverse)
library(dplyr)
library(readxl)
library(openxlsx)

##opens a window to select files, 
list_active = read.csv(choose.files(default = "_LISTOFACTIVE.CSV",caption = "Select Active Customer File",multi = FALSE,), skip = 1, header = FALSE, colClasses = c("character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL") ) #import MQ data
colnames(list_active) <- c("CUSTOMER_NBR","CONTRACT_NUMBER","ENTITY_CODE","ENTITY_NAME","LCO_CITY","LCO_STATE","FIRST_NAME","MIDDLE_NAME","LAST_NAME","STB","SC","SERVICE_NAME","SERVICE_CODE","CASCODE","PLAN_CODE","PLAN_NAME","BILLING_FREQUENCY","MOBILE_PHONE")
plan_names = read.csv(sprintf("https://spreadsheets.google.com/feeds/download/spreadsheets/Export?key=17fLf3_5nMKuOZxMvKY_baJjD3G8l-KKHxw3WSTNKh6o&exportFormat=csv"))
bouquet_names = read.csv(sprintf("https://drive.google.com/u/0/uc?id=1iHErLr_cL36BWzYwsQjOId-YlWWWbAr1&export=download"))
list_bc = read.csv(choose.files(default = "_PACK_DETAILS.CSV",caption = "Select Package File",multi = FALSE,),)

list_bc = list_bc %>% select(Service.Code,Broadcaster) %>% unique()
colnames(list_bc)[1] <- "SERVICE_CODE"

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

###PLANWISE SEGREGATION
list_ac_plan = filter(list_active_area, PLAN_NAME %in% plan_names$Plan.Name) %>% select(CUSTOMER_NBR,ENTITY_CODE,ENTITY_NAME,Area,STB,VC,PLAN_NAME) %>% unique()
ac_plan_pivot = list_ac_plan %>% group_by(Area,PLAN_NAME) %>% summarize(Active.Cust = n())
write.csv(ac_plan_pivot,"AreawisePlan.csv",row.names = F)

###alacarte segregate
list_ac_ala = filter(list_active_area, PLAN_NAME == "Alacarte Plan") %>% select(CUSTOMER_NBR,ENTITY_CODE,Area,SERVICE_NAME,SERVICE_CODE,PLAN_NAME) %>% unique()
list_ac_ala_bc = merge(list_ac_ala,list_bc,all.x = T)
#write.csv(list_ac_ala_bc,"Alacarte.csv",row.names = F)

list_ac_bq = filter(list_active_area, !(PLAN_NAME %in% plan_names$Plan.Name))
list_ac_bq = filter(list_ac_bq, SERVICE_NAME %in% bouquet_names$Bouquet) %>% select(CUSTOMER_NBR,ENTITY_CODE,Area,SERVICE_NAME,SERVICE_CODE,PLAN_NAME) %>% unique()
list_ac_bq_bc = merge(list_ac_bq,list_bc,all.x = T)
#write.csv(list_ac_bq_bc,"Bouquets.csv",row.names = F)

#add all data frames in a list with sheet names. requires openxlsx
list_of_sheets = list("Areawiseplan" = ac_plan_pivot,"Alacarte"=list_ac_ala_bc,"Bouquets"=list_ac_bq_bc)
write.xlsx(list_of_sheets, file = "Channel_penetration.xlsx")