library(tidyverse)
library(dplyr)
library(lubridate)

lcoCode = 'MD0030'
daterq = "2024-12-31"
listActive = read.csv(file.choose()) 
colnames(listActive)[10] <- "VC"
colnames(listActive)[11] <- "STB"

dueRenewal = read.csv(file.choose()) %>% select(Contract.Number,Contract.End.Date)
######make dueforrenewal dates proper
#dueRenewal$Contract.End.Date <- parse_date_time(dueRenewal$Contract.End.Date, orders = "dmy HMS")
dueRenewal$Contract.End.Date <- as.Date(dueRenewal$Contract.End.Date, "%d/%m/%Y")

LcoPricing = read.csv(file.choose()) 
LcoPricingFilter = LcoPricing %>% filter(Entity.Code %in% lcoCode) %>% filter(Lco.Price.Status == 'A') %>% filter(!(Plan.Name == 'Alacarte (Promotional)'))
# Copy values from 'Service.Code' to 'Plan.Code' where 'Plan.category' is 'Alacarte'
LcoPricingFilter$Plan.Code[LcoPricingFilter$Plan.Category == 'Alacarte'] <- LcoPricingFilter$Service.Code[LcoPricingFilter$Plan.Category == 'Alacarte']
LCoFinalPricing = LcoPricingFilter %>% select(Plan.Code,Price)
LCoFinalPricing$Plan.Code <- gsub("FTAPLAN","FTA",LCoFinalPricing$Plan.Code)

listActiveFilter = listActive %>% select(CUSTOMER_NBR,CONTRACT_NUMBER,ENTITY_CODE,ENTITY_NAME,STB,VC,SERVICE_CODE,SERVICE_NAME,BILLING_FREQUENCY) %>%
  filter(ENTITY_CODE %in% lcoCode) %>% filter(!(SERVICE_CODE == 'DPOBUNDLESERV')) %>% unique()
dueRenewalFilter = dueRenewal %>% filter(Contract.End.Date == daterq)
listActivewithEndDate = merge(listActiveFilter,dueRenewalFilter, by.x = 'CONTRACT_NUMBER',by.y = 'Contract.Number')
write.csv(listActivewithEndDate %>% select(CUSTOMER_NBR,BILLING_FREQUENCY) %>% unique(),"CUSTOMERS_END_BY_DATE.CSV",row.names = F)
LcoPackWithPrice = merge(listActivewithEndDate,LCoFinalPricing, by.x = 'SERVICE_CODE', by.y = 'Plan.Code')
#import account number and days for renewal data. manually make after 
renewReq = read.csv(file.choose())
LcoPackWithPriceFinal = merge(LcoPackWithPrice,renewReq)
LcoPackWithPriceFinal = LcoPackWithPriceFinal %>% mutate(FullAmnt = (Price/30)*Days)
LcoPackWithPriceFinal = LcoPackWithPriceFinal %>% mutate(FinalAmnt = FullAmnt*1.18)
write.csv(LcoPackWithPriceFinal,"LCO_Price_required_byDate.csv",row.names = F)
