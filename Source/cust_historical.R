library(readr)
library(tidyverse)
library(here)
library(dplyr)
cas_code = read.csv("4451414_ServiceDetails.CSV")
cust_historical <- read_csv("5079444_CUSTOMERS_NETWORK.CSV")
names(cust_historical)<-make.names(names(cust_historical),unique = TRUE)
cust_hist_trim = select(cust_historical,Customer.Nbr,Set.Box.Number,Smartcard.Serialnumber,Name,Mobile) %>% unique()



cust_historical <- cust_historical %>% mutate(VC.length = nchar(Smartcard.Serialnumber),  .after = 11)
cust_historical$VC.length <- gsub("16","ABV",cust_historical$VC.length, fixed = TRUE)
list_ac_ABV = filter(cust_historical, VC.length == "ABV") %>% select(Smartcard.Serialnumber,Package.Code)
list_ac_ABV_codes = merge(list_ac_ABV,cas_code,all.x = TRUE) %>% na.omit(list_ac_ABV_codes, cols='Provision.Code')
#VC_count = list_ac_ABV %>% select(Smartcard.Serialnumber) %>% unique()
write.csv(cust_hist_trim,"Customer_backdate.csv", row.names = F)
