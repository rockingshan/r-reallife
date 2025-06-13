library(tidyverse)
library(dplyr)
library(lubridate)

entity = "MD0030"
list_active = read.csv(file.choose(new = F) ) #import MQ data new list of Active NEW with autorenew
list_active = list_active %>% filter(!(Plan.Code == 'DPOPROMBUN')) %>% filter(Entity.Code == entity)
list_active_s = list_active %>% select(Customer.Nbr,Contract.Number,Entity.Code,First.Name,Last.Name,Stb,Sc,Service.Code,Service.Name,Mobile.Phone,Pri.Address1,Is.Auto.Renew) %>% unique()

duernw = read.csv(file.choose())
duernw_fl = duernw %>% select(Contract.Number,Contract.End.Date)
price = read.csv(file.choose()) 




lco_price = price %>% filter(Lco.Price.Status == "A") %>% filter(Entity.Code == entity) 
lco_price_a = lco_price %>% filter(Plan.Code %in% c("ALACARTEPL","ALACARTEPL_PR"))
lco_price_a$Plan.Code = lco_price_a$Service.Code
lco_price_b = lco_price %>% filter(!(Plan.Code %in% c("ALACARTEPL","ALACARTEPL_PR")))
lco_price_b$Plan.Code = gsub("FTAPLAN","FTA",lco_price_b$Plan.Code)
lco_price_n = rbind(lco_price_a,lco_price_b)

lco_price_for_cal = lco_price_n %>%  unite(combined, c("Entity.Code","Plan.Code")) %>% select(combined,Price) %>% unique()

list_active_s1 = list_active_s %>% unite(combined,c("Entity.Code","Service.Code"))
list_active_price = merge(list_active_s1, lco_price_for_cal,all.x = T)
list_active_price = list_active_price %>% mutate(PriceWtax = Price*1.18)
list_active_price$PriceWtax = round(list_active_price$PriceWtax, digits = 2)
active_price = list_active_price %>% group_by(Customer.Nbr) %>% summarise(TotalPriceLCO = sum(PriceWtax))

list_act_combine = list_active %>% group_by(Customer.Nbr) %>% summarise(Services=paste(Service.Name, collapse=","))
list_act_filter = list_active %>% select(Customer.Nbr,Contract.Number,First.Name,Last.Name,Stb,Sc,Mobile.Phone,Is.Auto.Renew) %>% unique()
total_active = merge(list_act_combine,list_act_filter)
total_act_date = merge(total_active,duernw_fl,all.x = T)
total_act_date$Contract.End.Date = as.Date(total_act_date$Contract.End.Date,  "%d/%m/%Y")
total_act_date_price = merge(total_act_date,active_price,all.x = T)
total_act_date_price = total_act_date_price[order(total_act_date_price$Contract.End.Date), ]
write.csv(total_act_date_price, paste0(entity,"_customer_enddate_w_wallet_requirement.csv"),row.names = F)

#### LCO balance deduction calculation ####

lco_price = price %>% filter(Lco.Price.Status == "A") %>% filter(Entity.Code == "MD0508") 
lco_price_a = lco_price %>% filter(Plan.Code == "ALACARTEPL")
lco_price_a$Plan.Code = lco_price_a$Service.Code
lco_price_b = lco_price %>% filter(!(Plan.Code == "ALACARTEPL"))
lco_price_b$Plan.Code = gsub("FTAPLAN","FTA",lco_price_b$Plan.Code)
lco_price_n = rbind(lco_price_a,lco_price_b)

# Save the dataframe to an RDS file
#saveRDS(lco_price_n, "price_508.rds")


###start from here
# Load the dataframe back into R
lco_price_n <- readRDS("price_508.rds")

lco_price_for_cal = lco_price_n %>% select(Plan.Code,Price) %>% unique()
CUSTOMER = read.csv(file.choose())  ##customer for calculation #only Customer.Nbr column
list_active_price = merge(CUSTOMER, list_active_s) %>% unique()
list_active_price_cal = merge(list_active_price,lco_price_n, by.x = 'Service.Code',by.y = 'Plan.Code') %>%
  select(Customer.Nbr,Contract.Number,Service.Name.x,Price)
total_act_date = merge(list_active_price_cal,duernw_fl,all.x = T)
total_act_date$Contract.End.Date = as.Date(total_act_date$Contract.End.Date,  "%d/%m/%Y")
total_act_date = total_act_date %>% mutate(PriceWtax = Price*1.18)
total_act_date$PriceWtax = round(total_act_date$PriceWtax, digits = 2)
total_act_date = total_act_date %>% mutate(Remain = ((Contract.End.Date - today()) + 1))
total_act_date$Remain = as.numeric(total_act_date$Remain)
total_act_date = total_act_date %>% mutate(Calc = (PriceWtax/30)*Remain)

write.csv(total_act_date,"Output/Debit_requirement.csv")
