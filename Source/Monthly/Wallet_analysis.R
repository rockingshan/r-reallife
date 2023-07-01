library(tidyverse)
library(dplyr)
library(readxl)
library(stringr)
library(lubridate)

#function definitions....

area_wise_op <- function(wallet_in){
  # get wallet input and filter on starting condition and export areawise
  WALLET_AREA_BPC = wallet_in %>% filter(str_detect(Entity.Code, "MSW"))
  write.csv(WALLET_AREA_BPC, "Output/Berhampore.csv", row.names = FALSE)
  WALLET_AREA_HLZ = wallet_in %>% filter(str_detect(Entity.Code, "HCS"))
  write.csv(WALLET_AREA_HLZ, "Output/Haldia.csv", row.names = FALSE)
  zip_area_files = paste(normalizePath(dirname(list.files(path = "Output/", pattern = "\\.csv$", ignore.case = TRUE,full.names = T))),fsep= .Platform$file.sep,list.files(path = "Output/", pattern = "\\.csv$", ignore.case = TRUE),sep="")
  zip(zipfile = sprintf("Output/Berhampore_Haldia_%s_%g",month(today() - months(1),label = TRUE, abbr = F),year(today())), files = zip_area_files, flags = " a -tzip -sdel",
      zip = "C:\\Program Files\\7-Zip\\7Z")
}

plan_wise_op <- function(wallet_in){
  #filter plan wise and alacarte and other and output
  wallet_filtered_ala = filter(wallet_in, Plan.Details=="Alacarte Plan")
  write.csv(wallet_filtered_ala, "Alacarte.csv", row.names = FALSE)
  wallet_filtered_othr = filter(wallet_in, Plan.Details!="Alacarte Plan")
  write.csv(wallet_filtered_othr, "PlanBouqets.csv", row.names = FALSE)
}

lco_pivot_table <- function(wallet_in){
  wallet_filt = filter(wallet_in, Credit.Document.Type=="INVOICE") %>% select(Entity.Code,Amount.Debit)
  ### Following blocks summarises code wise debit amount and export file --pivot table
  lco_pivot = wallet_filt %>% 
    group_by(Entity.Code) %>%
    summarize(Total_debit = sum(Amount.Debit))
  write.csv(lco_pivot, sprintf("Output/LCOWISE_WALLET_SUMMARY_%s_%g.csv",month(today() - months(1),label = TRUE, abbr = F),year(rollback(today()))), row.names = FALSE)
}

lcowise_data_export <- function(wallet_in){
  ## filter lco code and remove duplicate then convert to a list for loop
  lco_list = wallet_in %>% select(Entity.Code) %>% distinct()
  lco_list = lco_list[['Entity.Code']]
  
  ## run the loop according to the list and export csv for each LCO
  for (lcocode in lco_list) {
    wallet_filtered = filter(wallet_in, Entity.Code==lcocode)
    write.csv(wallet_filtered, sprintf("Output/%s_%s_%g.csv",lcocode,month(today() - months(1),label = TRUE, abbr = F),year(rollback(today()))), row.names = FALSE)
    zip_lco_files = paste(normalizePath(dirname(list.files(path = "Output/", pattern = "\\.csv$", ignore.case = TRUE,full.names = T))),fsep= .Platform$file.sep,list.files(path = "Output/", pattern = "\\.csv$", ignore.case = TRUE),sep="")
    zip(zipfile = sprintf("Output/LCOWise_Wallet_Report_%s_%g",month(today() - months(1),label = TRUE, abbr = F),year(rollback(today()))), files = zip_lco_files, flags = " a -tzip -sdel",
        zip = "C:\\Program Files\\7-Zip\\7Z")
  }
 
}

hdnd_nm = c('MDKH','MDBKT','MDBQA','MDCNDP','MDDHH','MDHCNJV','MDOR','MDSKWJV','TESTENTITY','CORP')


wallet = read.csv(file.choose(new = F),colClasses = c(Unique.Id="character"))
wallet = filter(wallet, !(Entity.Code %in% hdnd_nm))

area_wise_op(wallet)

#plan_wise_op(wallet)

lcowise_data_export(wallet)

lco_pivot_table(wallet)



crdr = read.csv(file.choose(new = F))
crdr1 = crdr %>% filter(NOTE_TYPE %in% c("CR","DR"))
#crdr1 = crdr1 %>% filter(!(ENTITY_CODE %in% hdnd_nm))
write.csv(crdr1, sprintf("Output/Credit_Debit_Note_%s_%g.csv",month(today() - months(1),label = TRUE, abbr = F),year(today())), row.names = FALSE)


#######additional amount for odisha
#ODISHA ELITE @ 175
#ODISHA GOLD @ 230
#ODISHA POWER @_200
#5 rupees extra for MDOR09


#######with broadcaster
service = read.csv(file.choose(new = F)) ### service details file
plna_type = read.csv(file.choose(new = F)) ### plan type
wallet_broad = merge(wallet_filt,service,all.x = T,all.y = F)
wallet_final = merge(wallet_broad,plna_type)
wallet_ordered = wallet_final %>% select(Customer.Nbr,Customer.Name,Unique.Id,Entity.Code,Entity.Name,Mobile,
                                         Plan.Details,Type,Service.Name,Broadcaster,Amount.Debit,Transaction.Date,
                                         Contract.Number,Billing.Frequency)

write.csv(wallet_ordered,"July_2022_LCO_packagewise_bill.csv",row.names = F)
##monthwise packagewise data
# wallet_sel = filter(wallet,Plan.Details == "SILVER BUDGET DIGITAL @ 180") %>%
#   select(Customer.Nbr,Entity.Code,Entity.Name,Plan.Details,Transaction.Date) %>% unique()
# 
# write.csv(wallet_sel,"2.csv",row.names = F)
# account_view = filter(wallet, Customer.Nbr == "283569MD0299")
# view(account_view)
# 
# cus_bill = wallet %>% select(Customer.Nbr, Billing.Frequency) %>% distinct()
# write.csv(cus_bill, "cusbill.csv", row.names = FALSE)
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



####read files and filter and output#######
# file.list = list.files(path = "C:/Users/SHANTANU/Downloads/wallet/WALLET REPORTS", pattern = NULL, all.files = FALSE, full.names = TRUE) 
# 
# # for (filename in file.list) {
# #   file_cont = read.csv(filename)
#    wallet_filt = filter(wallet, Credit.Document.Type=="INVOICE") %>% select(Customer.Nbr,Customer.Name,Unique.Id,Entity.Code,Entity.Name,Mobile,Plan.Details,Service.Name,Amount.Debit,Transaction.Date,Contract.Number,Billing.Frequency)
#    write.csv(wallet_filt,"4317930_WALLETSUMMLCONEW.CSV",row.names = F)
# # }
####Find Direct customers bills####
direct_cus = c('MD0440','MBDML','MD0479','MD0478')
wallet = filter(wallet, (Entity.Code %in% direct_cus))

wallet_filt = filter(wallet, Credit.Document.Type=="INVOICE") %>% select(Customer.Nbr,Customer.Name,Unique.Id,Entity.Code,Plan.Details,Service.Name,Amount.Debit,Billing.Frequency,Transaction.Date)
wallet_filt$Amount.Debit = round(wallet_filt$Amount.Debit,digits = 2)
df = wallet_filt %>% group_by(Plan.Details) %>% summarise(debit = sum(Amount.Debit))
customer_dt = wallet %>% group_by(Customer.Nbr) %>% summarise(Tot_debit = sum(Amount.Debit))
write.csv(df, "Planwise_amount.csv",row.names = F)

###monthwise
# Convert Transaction.Date column to a date-time object
wallet_filt$Transaction.Date <- dmy_hms(wallet_filt$Transaction.Date)

# Extract month and year from Transaction.Date column and combine into a new column
wallet_filt$Month.Year <- format(wallet_filt$Transaction.Date, "%Y-%m")

# Loop through each month and create separate data frames
for (my in unique(wallet_filt$Month.Year)) {
  month_data <- wallet_filt[wallet_filt$Month.Year == my, ]
  filename <- paste0("Meghbela_Subs_Bill_", my, ".csv")
  write.csv(month_data, filename, row.names = FALSE)
  
}

####broadcaster wise wallet####
service_bc = read.csv(file.choose())
wallet_service = wallet_filt %>% filter(!(Service.Name == '')) %>% select(Service.Name,Amount.Debit) %>%
  group_by(Service.Name) %>% summarise(Total_ded_W_TAX = sum(Amount.Debit))
wallet_serv_bc = merge(wallet_service,service_bc,all.x = T)
wallet_serv_bc = wallet_serv_bc %>% mutate(Broadcaster.part_WO_TAX = ((Total_ded_W_TAX/1.18)*0.889))
wallet_serv_bc$Broadcaster.part_WO_TAX = round(wallet_serv_bc$Broadcaster.part_WO_TAX,digits = 2)
write.csv(wallet_serv_bc,"SERVICEwise_amount_may23.csv")

wallet_plan = wallet_filt %>% filter((Service.Name == '')) %>% select(Plan.Details,Amount.Debit) %>%
  group_by(Plan.Details) %>% summarise(Total_ded_W_TAX = sum(Amount.Debit))
write.csv(wallet_plan,"PLANwise_amount_may23.csv")
