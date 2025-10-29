library(tidyverse)
library(dplyr)
library(readxl)
library(purrr)
library(stringr)
library(pivottabler)

list_bouquet_dated = read.csv(file.choose(new = F),colClasses = c(Service.Cas.Code="character")) #import MQ data bouquet
list_alacarte = read.csv(file.choose(new = F),colClasses = c(Service.Cas.Code="character")) #import MQ alacarte details
colnames(list_bouquet_dated)[5] <- "Bouquet_Channel"
colnames(list_bouquet_dated)[6] <- "Broadcaster.Name"
colnames(list_alacarte)[5] <- "Bouquet_Channel"
list_bouq_al = rbind(list_bouquet_dated,list_alacarte)
list_bouq_al$Smart.Card.Number <- gsub("'","",list_bouq_al$Smart.Card.Number)
list_bouq_al$Provisioning.Attribute <- gsub("'","",list_bouq_al$Provisioning.Attribute)

list_bouquet_dated$Smart.Card.Number <- gsub("'","",list_bouquet_dated$Smart.Card.Number)
list_bouquet_dated$Provisioning.Attribute <- gsub("'","",list_bouquet_dated$Provisioning.Attribute)
list_alacarte$Smart.Card.Number <- gsub("'","",list_alacarte$Smart.Card.Number)
list_alacarte$Provisioning.Attribute <- gsub("'","",list_alacarte$Provisioning.Attribute)

#getmultiple hardware
mulhw = list_bouq_al %>% select(Cust.Id,Provisioning.Attribute) %>% unique() 
mulhw$Cust.Id <- trimws(mulhw$Cust.Id)
mulhw$Provisioning.Attribute <- trimws(mulhw$Provisioning.Attribute)
mulwid = mulhw %>% group_by(Cust.Id) %>%
  filter(n() > 1) %>%
  ungroup() %>% select(Cust.Id) %>% unique()
bq_mul = merge(list_bouquet_dated,mulwid) %>% select(Cust.Id,Smart.Card.Number,Provisioning.Attribute,Bouquet_Channel,Month,Week,Service.Cas.Code) %>% unique()
al_mul = merge(list_alacarte,mulwid) %>% select(Cust.Id,Smart.Card.Number,Provisioning.Attribute,Bouquet_Channel,Month,Week,Service.Cas.Code) %>% unique()
write.csv(bq_mul,"Output/Bouquet_report_multiple_hardware_28092025.csv",row.names = F)
write.csv(al_mul,"Output/Alacarte_report_multiple_hardware_28092025.csv",row.names = F)

list_active = read.csv(file.choose(new = F),colClasses = c(CASCODE="character")) # MQ active report
list_active$STB <- gsub("'","",list_active$STB)
list_active$SC <- gsub("'","",list_active$SC)
colnames(list_active)[10] <- "VC"
colnames(list_active)[11] <- "STB"
 


actCount = list_active %>% filter(!(list_active$CASCODE == '')) %>% select(ENTITY_CODE,CUSTOMER_NBR) %>% unique() %>% group_by(ENTITY_CODE) %>% summarise(Count = n())
write.csv(actCount,"Customer_count.csv",row.names = F)


#import safeview data and searialze
list_sfw = read.csv(file.choose(new = F),colClasses = c(SubscriptionID="character")) #specify particular column as character
sfw_cas_data = list_sfw %>% unite(combined, c("SMCs","SubscriptionID"))

#import gospell data 
#### Gospell CAS file does not have column names. Please add column names, adding column names separately
list_gospell = read.csv(file.choose(new = F), header = FALSE)
colnames(list_gospell) <- c("vc", "cascode")
GSPL_cas_data = list_gospell %>% unite(combined, c("vc", "cascode"))
#GSPL_cas_data = GSPL_cas_data %>% mutate(cas = "GOSPELL")

##import ABV boxes
fil_path_abv = paste(normalizePath(dirname(list.files(,pattern = paste("CumulativeActiveSMCReport","*",sep = "")))),fsep= .Platform$file.sep,list.files(,pattern = paste("CumulativeActiveSMCReport","*",sep = "")),sep="")
sheets <-  excel_sheets(fil_path_abv)
data_sheets <- sheets[grepl("CumulativeActiveSMCReport", sheets)]
sheet_df <- map_dfr(data_sheets, ~read_excel(fil_path_abv, sheet = .x, skip = 1), id = .x)
abv_cas_data = filter(sheet_df, PPCSTATUS == "Activated")
abv_cas_data_combn = abv_cas_data %>% unite(combined, c("SMARTCARDNO","PACKAGEID"))







