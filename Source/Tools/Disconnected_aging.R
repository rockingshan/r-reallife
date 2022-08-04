library(tidyverse)
library(dplyr)

discon_list = read.csv(file.choose(new = F),colClasses = c(Vc="character")) #import daily disconencted customers files
colnames(discon_list)[1]<-"Customer.Nbr" #allign name with wallet fle
wallet_list = read.csv(file.choose(new = F)) #import last few days wallet report
plan_names = read.csv(sprintf("https://spreadsheets.google.com/feeds/download/spreadsheets/Export?key=17fLf3_5nMKuOZxMvKY_baJjD3G8l-KKHxw3WSTNKh6o&exportFormat=csv"))
colnames(wallet_list)[8]<-"Plan.Name" #change column name to match online file name
wallet_list_filt = filter(wallet_list, Plan.Name %in% plan_names$Plan.Name) %>% select(Customer.Nbr,Transaction.Date) #filter wallet for base plans
discon_transact = merge(discon_list,wallet_list_filt,all.x = T)
#following converts datetime to date and difference and put in a column
discon_transact$date_diff <- as.Date(as.POSIXct(discon_transact$Transaction.Date, format = "%d/%m/%Y %I:%M:%S %p"))-as.Date(as.POSIXct(discon_transact$Disconnection.Date, format = "%d/%m/%Y %I:%M:%S %p"))
#first group by date diff , then count total occurences, then add column of hta totals pecentage
discon_pivot = discon_transact %>% group_by(date_diff) %>% summarize(count_ = n()) %>% mutate(percentage = count_/sum(count_)*100)
discon_isna = discon_transact %>% filter(is.na(date_diff))
write.csv(discon_isna,"Output/NotRenewed.csv",row.names = F)
