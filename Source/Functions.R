library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
#library(janitor)
#library(httr)
library(xlsx)

####MQ list of active customers report import
mq_active_report <- function(){
  list_active = read.csv(file.choose(new = F), skip = 1, header = FALSE, colClasses = c("character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","character","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL") ) #import MQ data
  colnames(list_active) <- c("CUSTOMER_NBR","CONTRACT_NUMBER","ENTITY_CODE","ENTITY_NAME","LCO_CITY","LCO_STATE","FIRST_NAME","MIDDLE_NAME","LAST_NAME","STB","SC","SERVICE_NAME","SERVICE_CODE","CASCODE","PLAN_CODE","PLAN_NAME","BILLING_FREQUENCY","MOBILE_PHONE","EMAIL","HOME_PHONE","PRI_STATE","PRI_CITY","PRI_ADDRESS1")
  list_active$STB <- gsub("'","",list_active$STB)
  list_active$SC <- gsub("'","",list_active$SC)
  colnames(list_active)[10] <- "VC"
  colnames(list_active)[11] <- "STB"
  list_active <- list_active %>% mutate(VC.length = nchar(VC),  .after = 11) # get character length of vc
  list_active$VC.length <- gsub("8","GOSPELL",list_active$VC.length, fixed = TRUE)
  list_active$VC.length <- gsub("12","SAFEVIEW",list_active$VC.length, fixed = TRUE)
  list_active$VC.length <- gsub("16","ABV",list_active$VC.length, fixed = TRUE) #REPLACE LENGTHS TO CAS NAMES
  return(list_active)
}

abv_data_import <- function(){
  fil_path_abv = paste(normalizePath(dirname(list.files(,pattern = paste("CASEntitlementDumpReport","*",sep = "")))),fsep= .Platform$file.sep,list.files(,pattern = paste("CASEntitlementDumpReport","*",sep = "")),sep="")
  sheets <-  excel_sheets(fil_path_abv)
  data_sheets <- sheets[grepl("CASEntitlement", sheets)]
  sheet_df <- map_dfr(data_sheets, ~read_excel(fil_path_abv, sheet = .x, skip = 1), id = .x)
  abv_cas_data = filter(sheet_df, STATUS == "Activated")
  #abv_cas_data = read_xlsx(choose.files(default = "",caption = "Select ABV CAS File",multi = FALSE,))
  abv_cas_data_combn = abv_cas_data %>% unite(combined, c("SMARTCARDNO","PACKAGEID"))
  return(abv_cas_data_combn)
}

send_daily_sms <- function(){
  ##############send sms - autorenewal\
  due_frwn = read.csv(file.choose(new = F))
  due_frwn$Mobile.Phone <- as.numeric(due_frwn$Mobile.Phone)
  due_frwn <- due_frwn %>% mutate(mob.len = nchar(Mobile.Phone),  .after = 5)
  due_frwn <- due_frwn %>% filter(mob.len == 10)
  df <- data.frame(matrix(ncol = 2, nrow = 0))
  due_frwn$Contract.End.Date <- parse_date_time(due_frwn$Contract.End.Date, orders = "dmy HMS")
  due_frwn$Contract.End.Date <- as.Date(due_frwn$Contract.End.Date)
  due_frwn_flt = due_frwn %>% filter(Contract.End.Date == lubridate::today()) %>% select(Customer.Number,Mobile.Phone)
  for (i in row.names(due_frwn_flt)) {
    readurl = read_lines(paste("http://1.rapidsms.co.in/api/push.json?apikey=60461d4b29af2&route=trans&sender=MCBSPL&mobileno=",due_frwn_flt[i,"Mobile.Phone"],"&text=Dear%20Customer%2C%20Your%20CABLE%20TV%20plan%20for%20account%20",due_frwn_flt[i,"Customer.Number"],"%20is%20expiring%20Today%20.%20Recharge%20now%20to%20avoid%20interruption%20-%20MEGHBELA",sep = ""))
    df[nrow(df) + 1,] = readurl
 }
  
}