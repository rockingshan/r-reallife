library(tidyverse)
library(dplyr)
library(lubridate)


transactions_df <- read.csv(file.choose(),na.strings=c(""," ","NA")) ###get lco wallet bills report
active_services_df <- read.csv(file.choose()) #get list of active
due_rn <- read.csv(file.choose()) %>% select(Contract.Number,Contract.End.Date) #get due for renewal 
active_services_df <- merge(active_services_df,due_rn,by.x = "CONTRACT_NUMBER", by.y = "Contract.Number")
colnames(active_services_df)[27] <- "END_DATE"
active_services_df <- active_services_df %>% select("CUSTOMER_NBR", "CONTRACT_NUMBER", "ENTITY_CODE", "SERVICE_NAME", "SERVICE_CODE", "PLAN_NAME", "END_DATE")
transactions_df <- transactions_df %>% select("Customer.Nbr", "Customer.Name", "Unique.Id", "Entity.Code", "Plan.Details", "Service.Name", "Amount.Debit", "Transaction.Date", "Contract.Number", "Bill.Charge.Start.Date", "Bill.Charge.End.Date")
transactions_df1 <- transactions_df %>% filter(!(is.na(Contract.Number)))
transactions_df2 = transactions_df1 %>% filter(!(is.na(Service.Name)))
transactions_df3 = transactions_df1 %>% filter((is.na(Service.Name)))
transactions_df3$Service.Name = transactions_df3$Plan.Details
transactions_df3$Service.Name = gsub("Bronze basic","Bronze Basic",transactions_df3$Service.Name)
transactions_df3$Service.Name = gsub("DD Channels","DD",transactions_df3$Service.Name)
transactions_df3$Service.Name = gsub("\\(|\\)","",transactions_df3$Service.Name)

transactions_df = rbind(transactions_df2,transactions_df3)

#transactions_df2$Transaction.Date = dmy_hm(transactions_df2$Transaction.Date)
# Clean and convert date columns
transactions_df <- transactions_df %>%
  mutate(
    Bill.Charge.Start.Date = as.Date(Bill.Charge.Start.Date, format = "%d/%m/%Y %I:%M:%S %p"),
    Bill.Charge.End.Date = as.Date(Bill.Charge.End.Date, format = "%d/%m/%Y %I:%M:%S %p"),
    Transaction.Date = as.Date(Transaction.Date, format = "%d/%m/%Y %I:%M:%S %p")
  )

active_services_df <- active_services_df %>%
  mutate(
    END_DATE = as.Date(END_DATE, format = "%d/%m/%Y")
  )
# Filter to keep only the latest transaction for each service of each customer
df_latest_transactions <- transactions_df %>%
  group_by(Customer.Nbr, Service.Name, Contract.Number) %>%
  filter(Bill.Charge.End.Date == max(Bill.Charge.End.Date)) %>%
  ungroup()
# Convert CONTRACT_NUMBER and Contract.Number to character
df_latest_transactions <- df_latest_transactions %>%
  mutate(Contract.Number = as.character(Contract.Number))

active_services_df <- active_services_df %>%
  mutate(CONTRACT_NUMBER = as.character(CONTRACT_NUMBER))

# Join dataframes to match active services with the latest transactions
df_merged <- active_services_df %>%
  left_join(df_latest_transactions, 
             by = c("CUSTOMER_NBR" = "Customer.Nbr", 
                    "SERVICE_NAME" = "Service.Name", 
                    "CONTRACT_NUMBER" = "Contract.Number"))

# Identify services with invalid transaction validity
df_invalid <- df_merged %>%
  filter(!(END_DATE >= Bill.Charge.Start.Date & END_DATE <= Bill.Charge.End.Date))
# Find unmatched active services
df_unmatched <- active_services_df %>%
  anti_join(df_latest_transactions, 
            by = c("CUSTOMER_NBR" = "Customer.Nbr", 
                   "SERVICE_NAME" = "Service.Name", 
                   "CONTRACT_NUMBER" = "Contract.Number"))
# Combine unmatched and invalid services
df_services_without_proper_transaction <- bind_rows(df_unmatched, df_invalid)

# Write the output to a file
write.csv(df_services_without_proper_transaction, "Output/services_without_proper_transaction.csv", row.names = FALSE)

