library(tidyverse)
library(here)
library(dplyr)
library(readxl)
wallet = read.csv(here("data/4040924_WALLETSUMMLCONEW_NOV.CSV"))

wallet_difference_prev = read.csv(here("data/LcoWiseWalletDebitDifference.CSV"))[,2:5]



# wallet_mod = read_excel(here("", "WALLET REPORT OCT2020.xlsx"))
# names(wallet_mod) = make.names(names(wallet_mod))

### Following block filters and export files based on filter criteria
wallet_filtered = filter(wallet, Plan.Details!="Alacarte Plan")
# target = c("Star Jalsha @ 19", "Jalsha Movies @ 6")
# wallet_filter_condition = filter(wallet_filtered, Service.Name %in% target)
# export_condition = wallet_filter_condition %>% select(Customer.Nbr,Customer.Name,Unique.Id,Entity.Code,Entity.Name,Mobile,Service.Name)
write.csv(wallet_filtered, "WithoutAlacarte.csv", row.names = FALSE)

### Following blocks summarises code wise debit amount and export file --pivot table
lco_pivot = wallet %>% 
  group_by(Entity.Code) %>%
  summarize(Total_debit = sum(Amount.Debit))

# lco_pivot_modified = wallet_mod %>% 
#   group_by(Entity.Code) %>%
#   summarize(Total_debit.shantanu = sum(Amount.Debit))

### vlookup 
#difference_debit = merge(lco_pivot_modified,lco_pivot) %>% mutate(difference = Total_debit.shantanu-Total_debit.mq)
wallet_final_nov= merge(lco_pivot, wallet_difference_prev) %>% 
  mutate(New.Wallet.total = Total_debit - difference) %>% 
  select(Entity.Code,Total_debit,difference,New.Wallet.total)

### export
write.csv(wallet_final_nov, "LCO WALLET SUMMARY NOV20.csv", row.names = FALSE)

## filter lco code and remove duplicate then convert to a list for loop
lco_list = wallet %>% select(Entity.Code) %>% distinct()
lco_list = lco_list[['Entity.Code']]

## run the loop according to the list and export csv for each LCO
for (lcocode in lco_list) {
  wallet_filtered = filter(wallet_end_date, lco==lcocode)
  write.csv(wallet_filtered, paste(lcocode,".csv"), row.names = FALSE)
  }