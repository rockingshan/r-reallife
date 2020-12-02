library(tidyverse)
library(here)
library(dplyr)
library(readxl)
library(stringr)

#function definitions....

area_wise_op <- function(wallet_in){
  # get wallet input and filter on starting condition and export areawise
  WALLET_AREA_BPC = wallet_in %>% filter(str_detect(Entity.Code, "MSW"))
  write.csv(WALLET_AREA_BPC, "Berhampore.csv", row.names = FALSE)
  WALLET_AREA_HLZ = wallet_in %>% filter(str_detect(Entity.Code, "HCS"))
  write.csv(WALLET_AREA_HLZ, "Haldia.csv", row.names = FALSE)
}

plan_wise_op <- function(wallet_in){
  #filter plan wise and alacarte and other and output
  wallet_filtered_ala = filter(wallet_in, Plan.Details=="Alacarte Plan")
  write.csv(wallet_filtered_ala, "Alacarte.csv", row.names = FALSE)
  wallet_filtered_othr = filter(wallet_in, Plan.Details!="Alacarte Plan")
  write.csv(wallet_filtered_othr, "PlanBouqets.csv", row.names = FALSE)
}

lco_pivot_table <- function(wallet_in){
  ### Following blocks summarises code wise debit amount and export file --pivot table
  lco_pivot = wallet_in %>% 
    group_by(Entity.Code) %>%
    summarize(Total_debit = sum(Amount.Debit))
  write.csv(lco_pivot, "LCO WALLET SUMMARY.csv", row.names = FALSE)
}

lcowise_data_export <- function(wallet_in){
  ## filter lco code and remove duplicate then convert to a list for loop
  lco_list = wallet_in %>% select(Entity.Code) %>% distinct()
  lco_list = lco_list[['Entity.Code']]
  
  ## run the loop according to the list and export csv for each LCO
  for (lcocode in lco_list) {
    wallet_filtered = filter(wallet_in, Entity.Code==lcocode)
    write.csv(wallet_filtered, paste(lcocode,".csv", sep = ""), row.names = FALSE)
  }
}



wallet = read.csv(here("data/4040924_WALLETSUMMLCONEW_NOV.CSV"))

area_wise_op(wallet)

plan_wise_op(wallet)

lco_pivot_table(wallet)

lcowise_data_export(wallet)


# wallet_mod = read_excel(here("", "WALLET REPORT OCT2020.xlsx"))
# names(wallet_mod) = make.names(names(wallet_mod))

### Following block filters and export files based on filter criteria

# target = c("Star Jalsha @ 19", "Jalsha Movies @ 6")
# wallet_filter_condition = filter(wallet_filtered, Service.Name %in% target)
# export_condition = wallet_filter_condition %>% select(Customer.Nbr,Customer.Name,Unique.Id,Entity.Code,Entity.Name,Mobile,Service.Name)



# lco_pivot_modified = wallet_mod %>% 
#   group_by(Entity.Code) %>%
#   summarize(Total_debit.shantanu = sum(Amount.Debit))

### vlookup 
#difference_debit = merge(lco_pivot_modified,lco_pivot) %>% mutate(difference = Total_debit.shantanu-Total_debit.mq)

####following code is used if there are and adjustments need to made to the actual data
# wallet_final_nov= merge(lco_pivot, wallet_difference_prev) %>% 
#   mutate(New.Wallet.total = Total_debit - difference) %>% 
#   select(Entity.Code,Total_debit,difference,New.Wallet.total)

### export
