library(readr)
library(tidyverse)
library(dplyr)

cust_historical_start <- read.csv(file.choose(new = F),colClasses = c("character","character","NULL","character","character","NULL","NULL","character","character","NULL","character","character","character","character","character","character","character","NULL"))
cust_historical_start <- cust_historical_start %>% mutate(VC.length = nchar(Smartcard.Serialnumber),  .after = 4)
cust_historical_start$VC.length <- gsub("8","GOSPELL",cust_historical_start$VC.length, fixed = TRUE)
cust_historical_start$VC.length <- gsub("12","SAFEVIEW",cust_historical_start$VC.length, fixed = TRUE)
cust_historical_start$VC.length <- gsub("16","ABV",cust_historical_start$VC.length, fixed = TRUE) #REPLACE LENGTHS TO CAS NAMES
###############
cust_historical_end <- read.csv(file.choose(new = F),colClasses = c("character","character","NULL","character","character","NULL","NULL","character","character","NULL","character","character","character","character","character","character","character","NULL"))
cust_historical_end <- cust_historical_end %>% mutate(VC.length = nchar(Smartcard.Serialnumber),  .after = 4)
cust_historical_end$VC.length <- gsub("8","GOSPELL",cust_historical_end$VC.length, fixed = TRUE)
cust_historical_end$VC.length <- gsub("12","SAFEVIEW",cust_historical_end$VC.length, fixed = TRUE)
cust_historical_end$VC.length <- gsub("16","ABV",cust_historical_end$VC.length, fixed = TRUE) #REPLACE LENGTHS TO CAS NAMES
###############
# cust_hist_trim = select(cust_historical,Customer.Nbr,Set.Box.Number,Smartcard.Serialnumber,Name,Mobile) %>% unique()
# custhist_count = select(cust_hist_trim,Customer.Nbr) %>% unique()
# cust_historical <- cust_historical %>% mutate(VC.length = nchar(Smartcard.Serialnumber),  .after = 11)
# cust_historical$VC.length <- gsub("16","ABV",cust_historical$VC.length, fixed = TRUE)
# list_ac_ABV = filter(cust_historical, VC.length == "ABV") %>% select(Smartcard.Serialnumber,Package.Code)
# list_ac_ABV_codes = merge(list_ac_ABV,cas_code,all.x = TRUE) %>% na.omit(list_ac_ABV_codes, cols='Provision.Code')
# #VC_count = list_ac_ABV %>% select(Smartcard.Serialnumber) %>% unique()
# write.csv(cust_hist_trim,"Customer_backdate.csv", row.names = F)

#########################
#find customer decreased in this period
cust_start = cust_historical_start %>% select(Customer.Nbr,Set.Box.Number,STB.Item.Descr,Smartcard.Serialnumber,VC.length) %>% unique()
cust_end = cust_historical_end %>% select(Customer.Nbr,Set.Box.Number,STB.Item.Descr,Smartcard.Serialnumber,VC.length) %>% unique()
cust_drop = merge(cust_start,cust_end, by.x = 'Set.Box.Number', by.y = 'Set.Box.Number', all.x = T, all.y = F)
cust_drop_det = cust_drop[is.na(cust_drop$Customer.Nbr.y),]
cust_drop_piv = cust_drop_det %>% group_by(STB.Item.Descr.x) %>% summarise(Type.Count = n())
write.csv(cust_drop_piv,"STBMODEL_DEACTIVATED_YEARONYEAR.csv",row.names = F)
cust_drop_piv = cust_drop_det %>% group_by(VC.length.x) %>% summarise(Type.Count = n())
write.csv(cust_drop_piv,"CASL_DEACTIVATED_YEARONYEAR.csv",row.names = F)



###for schedule ix
cust_historical <- read.csv(file.choose(new = F),colClasses = c("character","character","NULL","character","character","NULL","NULL","character","character","NULL","character","character","character","character","character","character","character","NULL"))
cust_historical <- cust_historical %>% mutate(VC.length = nchar(Smartcard.Serialnumber),  .after = 4)
cust_historical$VC.length <- gsub("8","GOSPELL",cust_historical$VC.length, fixed = TRUE)
cust_historical$VC.length <- gsub("12","SAFEVIEW",cust_historical$VC.length, fixed = TRUE)
cust_historical$VC.length <- gsub("16","ABV",cust_historical$VC.length, fixed = TRUE) #REPLACE LENGTHS TO CAS NAMES
cust_historical_abv <- cust_historical %>% filter(VC.length == "ABV") %>% select(Smartcard.Serialnumber,Set.Box.Number,Package.Code,Package.Descr,Activation.date)
cascode = read.csv(file.choose(new = F))
cascode_abv = cascode %>% filter(Prov.Sys.Name == "ABV") %>% filter(Service.Status == "Active") %>% filter(Provsion.Status == "A")
cascode_abv$Provsion.Code <-  gsub("'","",cascode_abv$Provsion.Code)
colnames(cascode_abv)[1] <- "Package.Code"
cust_hist_abv_code = merge(cust_historical_abv,cascode_abv,all.x = T,all.y = F)
##cust_hist_abv_code = na.omit(cust_hist_abv_code)
cust_hist_abv_code1  = cust_hist_abv_code %>% filter(!(Package.Descr == "Ncf  Slab")) %>% select(Smartcard.Serialnumber,Set.Box.Number,Provsion.Code,Activation.date)
colnames(cust_hist_abv_code1)[4] <- "Package.Start.Date"
write.csv(cust_hist_abv_code1,"31102021.csv",row.names = F)
