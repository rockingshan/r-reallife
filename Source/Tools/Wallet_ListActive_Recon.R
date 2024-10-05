library(tidyverse)
library(dplyr)
library(lubridate)
library(janitor)


#### find discrepencies of active cust with wallet####
wallet_d =  read.csv(file.choose(new = F))
listactive = read.csv(file.choose(new = F))
wallet_d_base = filter(wallet_d, Plan.Details %in% plan_names$Plan.Name) %>% select(Customer.Nbr, Contract.Number, Plan.Details, Amount.Debit, Transaction.Date) %>% unique()
wallet_d_max = wallet_d_base %>% group_by(Customer.Nbr,Plan.Details) %>% mutate(Max.Date = max(Transaction.Date)) ##find last date of occurenece
wallet_d_max = wallet_d_max %>% select(Customer.Nbr, Contract.Number, Plan.Details, Amount.Debit, Max.Date)
wallet_d_max = wallet_d_max %>% unite(combined, c("Customer.Nbr","Plan.Details"))
wallet_d_max = wallet_d_max %>% select(combined,Max.Date) %>% unique()
active_c_flt = filter(list_active, PLAN_NAME %in% plan_names$Plan.Name) %>% select(CUSTOMER_NBR,PLAN_NAME,SC,BILLING_FREQUENCY) %>% unique()
active_c_flt = active_c_flt %>% unite(combined, c("CUSTOMER_NBR","PLAN_NAME"))
merge_res = merge(active_c_flt,wallet_d_max, all = T)
write.csv(merge_res, "pack to wallet.csv",row.names = F)