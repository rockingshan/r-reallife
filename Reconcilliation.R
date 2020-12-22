library(tidyverse)
library(here)
library(dplyr)
library(gapminder) 
list_active = read.csv(here("", "4113900_LISTOFACTCUST.CSV"))

list_sfw = read.csv(here("data","safeview.CSV"),colClasses = c(SubscriptionID="character")) #specify particular column as character
list_sfw$SubscriptionID = as.character(list_sfw$SubscriptionID)
sfw_cas_data = list_sfw %>% unite(combined, c("SMCs","SubscriptionID"))
## replace ' in column data, change to proper column names

list_active$STB <- gsub("'","",list_active$STB)
list_active$SC <- gsub("'","",list_active$SC)
colnames(list_active)[10] <- "VC"
colnames(list_active)[11] <- "STB"
list_active <- list_active %>% mutate(VC.length = nchar(VC),  .after = 11) # get character length of vc
list_active$VC.length <- gsub("8","GOSPELL",list_active$VC.length, fixed = TRUE)
list_active$VC.length <- gsub("12","SAFEVIEW",list_active$VC.length, fixed = TRUE)
list_active$VC.length <- gsub("16","ABV",list_active$VC.length, fixed = TRUE) #REPLACE LENGTHS TO CAS NAMES

list_ac_SFW = filter(list_active, VC.length == "SAFEVIEW")
list_ac_SFW = separate(list_ac_SFW, VC, into = c("leftval", "rightval"), sep = 10, remove = FALSE)
list_ac_SFW$leftval = as.numeric(list_ac_SFW$leftval)
list_ac_SFW = list_ac_SFW %>% unite(combined, c("leftval","CASCODE"))  
mq_sfw_data = list_ac_SFW %>% select(combined,CUSTOMER_NBR,VC,STB,SERVICE_NAME) %>% distinct()

reconcile_data = merge(x = sfw_cas_data, y = mq_sfw_data, by = "combined", all.x = TRUE)