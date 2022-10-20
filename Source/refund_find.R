library(tidyverse)
library(lubridate)
library(readxl)

history_data = read.csv(file.choose())
colnames(history_data) = colnames(history_data) %>% make.names()
wallet = read.csv(file.choose(),colClasses = c(Unique.Id ="character"))
#fcn_dis = dis_cust[grepl("MD0470",dis_cust$Customer.Number),]
fcn_disc = history_data %>% filter(User == 'F. C. N. Communication')
fcn_disc_refund = fcn_disc[grepl("Refund",fcn_disc$Description),] %>%
  select(Customer.Number,Transaction.Number,Transaction.Date)
fcn_wallet = wallet %>% select(Customer.Nbr,Unique.Id,Contract.Number,Amount.Debit,Transaction.Date,Billing.Frequency)
fcn_wallet = na.omit(fcn_wallet)
fcn_wallet = fcn_wallet %>%  mutate(across('Billing.Frequency', str_replace, ' Days', ''))
fcn_wallet$Billing.Frequency = as.numeric(fcn_wallet$Billing.Frequency)
fcn_wallet_discon = merge(fcn_wallet,fcn_disc_refund,by.x = 'Contract.Number',by.y = 'Transaction.Number',all.x = F,all.y = T)
fcn_wallet_na = fcn_wallet_discon %>% filter(is.na(Customer.Nbr))
fcn_wallet_discon = na.omit(fcn_wallet_discon)
fcn_wallet_discon$Transaction.Date.x = as.Date(as.POSIXct(fcn_wallet_discon$Transaction.Date.x, format = "%d/%m/%Y %I:%M:%S %p"))
fcn_wallet_discon$Transaction.Date.y = as.Date(as.POSIXct(fcn_wallet_discon$Transaction.Date.y, format = "%d-%m-%Y %H:%M:%S"))
fcn_wallet_final = fcn_wallet_discon %>% mutate(credit = (Amount.Debit/Billing.Frequency)*as.numeric((Billing.Frequency-(Transaction.Date.y-Transaction.Date.x))))
fcn_wallet_final = fcn_wallet_final %>% unite(combined, c("Customer.Nbr", "Contract.Number"))
fcn_wallet_final$credit = round(fcn_wallet_final$credit,digits = 2)
write.csv(fcn_wallet_final,"credit_fcn.csv")
write.csv(fcn_wallet_na,"_fcn_nafile.csv")


#####ANOTHER BLOCK
wallet_work = wallet %>% group_by(Customer.Nbr,Contract.Number) %>% summarise(Plan_Details = paste(Plan.Details, collapse = ","),Services = paste(Service.Name, collapse = ","),Wallet_Debit = sum(Amount.Debit))
write.csv(wallet_work,"FCN_WALLET.CSV",row.names = F)
