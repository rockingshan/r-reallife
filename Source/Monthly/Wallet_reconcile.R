library(tidyverse)
library(dplyr)
library(readxl)
library(stringr)
library(lubridate)

op_bal_open = read.csv(file.choose(new = F))
op_bal_close = read.csv(file.choose(new = F))
op_cr_dr_dtl = read.csv(file.choose(new = F))
op_payments_dtl = read.csv(file.choose(new = F))
op_wallet_c = read.csv(file.choose(new = F))


op_payments_dtl <- op_payments_dtl[!(op_payments_dtl$Entity.Code == ''),]  ###remove blank rows for particular column
op_payment = op_payments_dtl %>% filter(Party.Type == "OPERATIONAL ENTITY") %>% group_by(Entity.Code) %>% summarise(Total.Payments = sum(Amount))  ##make a pivot table of lco code and payment

op_cr_dr = op_cr_dr_dtl %>% group_by(ENTITY_CODE,NOTE_TYPE) %>% summarise(Adjustment = sum(ADJ_VALUE))
colnames(op_cr_dr)[1] <- "Entity.Code"
###split credit debit note file based on credit or debit
op_cr_dr <- op_cr_dr[,c(1,3,2)]
X <- split(op_cr_dr, op_cr_dr$NOTE_TYPE)
Y <- lapply(seq_along(X), function(x) as.data.frame(X[[x]])[, 1:2]) 
op_cr = Y[[1]]
op_dr = Y[[2]]


op_wallet_reco = merge(op_bal_open,op_payment, all.x = T)
colnames(op_wallet_reco)[4] <- "Opening.Balance"
colnames(op_wallet_reco)[5] <- "Balance.Type"
op_wallet_reco = merge(op_wallet_reco,op_cr,all.x = T)
colnames(op_wallet_reco)[7] <- "Credit.Note"
op_wallet_reco = merge(op_wallet_reco,op_dr,all.x = T)
colnames(op_wallet_reco)[8] <- "Debit.Note"
op_wallet_reco = merge(op_wallet_reco,op_wallet_c,all.x = T)
colnames(op_wallet_reco)[9] <- "Wallet.Consumption"
op_wallet_reco[is.na(op_wallet_reco)] <- 0
op_wallet_calc = op_wallet_reco %>% mutate(Calculated.Closing.Balance = (ifelse(Balance.Type == 'CR', (Opening.Balance*1),(Opening.Balance*-1)) + Total.Payments + Credit.Note - Debit.Note - Wallet.Consumption))
op_wallet_calc1 = merge(op_wallet_calc,op_bal_close,all.x = T)
op_wallet_calc1 = op_wallet_calc1 %>% mutate(Closing.MQ.Report = (ifelse(Note.Type == 'CR', (Balance*1),(Balance*-1))) )
op_wallet_calc1 = op_wallet_calc1 %>% mutate(Opening.MQ.Balance = (ifelse(Balance.Type == 'CR', (Opening.Balance*1),(Opening.Balance*-1))), .after = 5)
op_wallet_calc1 = op_wallet_calc1 %>% select(Entity.Code,Entity.Name,City,Opening.MQ.Balance,Total.Payments,Credit.Note,Debit.Note,
                                             Wallet.Consumption,Calculated.Closing.Balance,Closing.MQ.Report) %>% mutate(Difference = Closing.MQ.Report - Calculated.Closing.Balance )
#file_prefix <- readline("Enter a file name prefix: ")
write.csv(op_wallet_calc1,"JAN23_WALLET_RECONCILE.CSV",row.names = F)
