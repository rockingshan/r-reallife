library(tidyverse)
library(here)
library(dplyr)
library(gapminder) 
library(readxl)

cas_codes = read_excel("MQ Service and provision codes.xls")
price_plan = read_excel("81032.xls", col_types = c("guess","guess","guess","guess","guess","guess","guess","text","numeric","numeric"))

try = merge(x=price_plan,y=cas_codes,by="SERVICE_CODE", all.x = T, all.y = TRUE,)

write.csv(try, "PRICE_PLAN_CAS_CODES.CSV", row.names = FALSE)
###############################
inventory = read.csv(here("C:/Users/Shantanu/Downloads","4178337_INVENTDATA.CSV"), colClasses = c(SERIAL_NUMBER="character"))
lco_master = read.csv(here("C:/Users/Shantanu/Downloads","4175967_LCOMasterReport.CSV"))
aging = read.csv(here("C:/Users/Shantanu/Downloads","accounts_old.csv"))
colnames(aging)[1] <- "CUSTOMER_NBR"
inventory_w = inventory[!(inventory$LOCATION_DESCR=="" | inventory$LOCATION_DESCR=="Meghbela Store" ),]
lco_master_a = lco_master %>% select(LOCATION_DESCR,Lco.Code)
inventory_w = merge(x=inventory_w, y=lco_master_a, by="LOCATION_DESCR", all.x = T)
write.csv(inventory_w,"INVENTORY_WORK.CSV",row.names = F)

inventory_acc = inventory[(inventory$LOCATION_DESCR==""),]
inventory_acc = merge(x=inventory_acc,y=aging,by="CUSTOMER_NBR",all.x = F)
write.csv(inventory_acc,"INVENTORY_acc.CSV",row.names = F)



