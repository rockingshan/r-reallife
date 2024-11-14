library(tidyverse)
library(dplyr)
library(lubridate)

list_active = read.csv(file.choose(new = F) ) #import MQ data new list of Active with autorenew
list_active = list_active %>% filter(!(Plan.Code == 'DPOPROMBUN'))
list_active_s = list_active %>% select(Customer.Nbr,Contract.Number,Entity.Code,First.Name,Last.Name,Stb,Sc,Service.Code,Service.Name,Mobile.Phone,Pri.Address1,Is.Auto.Renew) %>% unique()

duernw = read.csv(file.choose())
duernw_fl = duernw %>% select(Contract.Number,Contract.End.Date)
price = read.csv(file.choose()) 
lco_price = price %>% filter(Lco.Price.Status == "A") %>% filter(Entity.Code == "MBK109") 
lco_price_a = lco_price %>% filter(Plan.Code == "ALACARTEPL")
lco_price_a$Plan.Code = lco_price_a$Service.Code
lco_price_b = lco_price %>% filter(!(Plan.Code == "ALACARTEPL"))
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
write.csv(total_act_date_price, "L&G_customer_enddate_w_wallet_requirement.csv",row.names = F)
