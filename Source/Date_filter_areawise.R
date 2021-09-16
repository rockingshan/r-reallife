library(tidyverse)
library(dplyr)
library(lubridate)

list_active = read.csv(choose.files(default = "_LISTOFACTIVE.CSV",caption = "Select Active Customer File",multi = FALSE,), skip = 1, header = FALSE, colClasses = c("character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL") ) #import MQ data
colnames(list_active) <- c("CUSTOMER_NBR","CONTRACT_NUMBER","ENTITY_CODE","ENTITY_NAME","LCO_CITY","LCO_STATE","FIRST_NAME","MIDDLE_NAME","LAST_NAME","STB","SC","SERVICE_NAME","SERVICE_CODE","CASCODE","PLAN_CODE","PLAN_NAME","BILLING_FREQUENCY","MOBILE_PHONE")
inventory = read.csv(choose.files(default = "_INVENTORY.CSV",caption = "Select Inventory File",multi = FALSE,),colClasses = c(SERIAL_NUMBER="character"))
inventory_select = select(inventory, SERIAL_NUMBER,ITEM_DESCR)
due_for_renewal = read.csv(choose.files(default = "_DUEFORRENEWAL.CSV",caption = "Select DueforRenewal File",multi = FALSE,))
dueforrenewal_select = select(due_for_renewal, Customer.Number,Contract.Number,Contract.End.Date) %>% unique()
list_active$STB <- gsub("'","",list_active$STB)
list_active$SC <- gsub("'","",list_active$SC)
colnames(list_active)[10] <- "VC"
colnames(list_active)[11] <- "STB"
list_active_hlz_cndp = list_active %>% filter(str_detect(ENTITY_CODE, c("HCS","MDCH"))) %>% select(CUSTOMER_NBR,CONTRACT_NUMBER,ENTITY_CODE,ENTITY_NAME,VC,STB) %>% unique()
list_active_with_date = merge(x=list_active_hlz_cndp,y=dueforrenewal_select, by.x = 'CONTRACT_NUMBER',by.y = 'Contract.Number',all.x = TRUE)
list_active_with_date$Contract.End.Date <- parse_date_time(list_active_with_date$Contract.End.Date, orders = "dmy HMS")
list_active_with_date$Contract.End.Date <- as.Date(list_active_with_date$Contract.End.Date)
#list_active_date = separate(list_active_with_date, col = Contract.End.Date, into = c("Date","Time","HR"),sep = " ",fill = "right")
list_active_serial = merge(list_active_with_date,inventory_select, by.x = 'STB',by.y = 'SERIAL_NUMBER', all.x = TRUE)
list_active_date_filtered = filter(list_active_serial, Contract.End.Date >= as.Date("2021-05-28"))

write.csv(list_active_date_filtered, "CNDP_HLZ.csv", row.names = F)
