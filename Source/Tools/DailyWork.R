library(tidyverse)
library(dplyr)
library(readxl)
library(stringr)
library(knitr)
#library(kableExtra)
library(janitor)
library(lubridate)


usrData = read.csv(file.choose())
userNames = c("AKASH RAM","AYON DAS","Ayantika Lahiri","AMARNATH KUNDU","ABHIJIT CHAKRABORTY","DEBASISH BISWAS","SUDIPTA DASGUPTA")

smsUserData = usrData %>% filter(User %in% userNames)
smsUserData$Transaction.Date = mdy_hms(smsUserData$Transaction.Date)
smsUserData$Transaction.Date <- format(smsUserData$Transaction.Date, "%m/%d/%Y %H:%M:%S")
smsUserData$Transaction.Date <- as.POSIXct(smsUserData$Transaction.Date, format = "%m/%d/%Y %H:%M:%S")
smsUserData <- smsUserData %>% mutate(since_midnight = hour(Transaction.Date) * 60 + minute(Transaction.Date)) %>% filter(since_midnight >= 10)


#remove provisioning logs

smsUserData <- smsUserData[!grepl("^Provisioning", smsUserData$Description), ]


#remove rows where account number is null for one time sale
smsUserData = smsUserData[!(smsUserData$Customer.Number == "" & smsUserData$Transaction.Type == 'One Time Sale'), ]

#filter file with based on criteria
target = c("Add Contract","Add Credit Note","Create Customer","Disconnect Contract","HARDWARE CHANGE","Hardware Return","Modify Contract","Modify Customer","One Time Sale","Reconnect Contract","RETRACK","New Payment","ChangePwd")
smsUserData = filter(smsUserData, Transaction.Type %in% target) %>% distinct()

#make pivot table and order by user
userWorkCount = smsUserData %>% group_by(User,Transaction.Type) %>% summarize(Transaction.Count = n()) %>% 
  pivot_wider(names_from = User, values_from = Transaction.Count)
#userWorkCount <- userWorkCount[order(userWorkCount[,1]),]

userWorkCount[is.na(userWorkCount)] <- 0

#create grand total of rows and columns
userWorkCount = adorn_totals(userWorkCount, where = c("row","col"), fill = "-", na.rm = TRUE, name = "Grand Total")


write.csv(userWorkCount, "Output/SMS_Work_Count.csv", na = "",row.names = FALSE)
write.csv(smsUserData, "Output/SMS_Work_Details.csv", na = "",row.names = FALSE)




# user_work_count %>% 
#   kable() %>% 
#   kable_styling(bootstrap_options = "striped", 
#                 full_width = TRUE)

