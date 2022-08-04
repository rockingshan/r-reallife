library(tidyverse)
library(dplyr)
library(readxl)
library(stringr)
library(knitr)
#library(kableExtra)
library(janitor)
library(lubridate)

date_char = format(Sys.Date()-1, format="%d%m%Y") #previous date 

#create list from the files 
#file.list = list.files(path = "F:/drive2/Daily report/", pattern = paste(date_char,"*",sep = ""), all.files = FALSE, full.names = TRUE) #find file of yesterday
file.list = list.files(path = "Data/", pattern = ".xlsx", all.files = FALSE, full.names = TRUE) #find file all month
#create list from file names
df.list <- lapply(file.list, read_excel)

#merge files in one variable and clear names
all_user_data <- bind_rows(df.list, .id = "id")
names(all_user_data) = make.names(names(all_user_data))
all_user_data <- all_user_data %>% mutate(since_midnight = hour(Date) * 60 + minute(Date)) %>% filter(since_midnight >= 10)
all_user_data = all_user_data %>% select(Customer..,Transaction.type,Transaction.No,User.ID,Date,Description)


#####IF CUSTOMER VERY LESS
all_data_flt = all_user_data
#remove provisioning logs
all_data_flt = all_user_data %>% dplyr::filter(substr(Description,1,13) != 'IProvisioning') 

#remove rows where account number is null for one time sale
all_data_flt = all_data_flt[!(is.na(all_data_flt$Customer..) & all_data_flt$Transaction.type=="One Time Sale"),]

#filter file with based on criteria
target = c("Add Contract","Add Credit Note","Create Customer","Disconnect Contract","HARDWARE CHANGE","Hardware Return","Modify Contract","Modify Customer","One Time Sale","Reconnect Contract","RETRACK","New Payment","ChangePwd")
all_user_data_filt = filter(all_data_flt, Transaction.type %in% target) %>% distinct()

#make pivot table and order by user
user_work_count = all_user_data_filt %>% group_by(User.ID,Transaction.type) %>% summarize(Transaction_count = n()) %>% 
  pivot_wider(names_from = User.ID, values_from = Transaction_count)
user_work_count <- user_work_count[order(user_work_count[,1]),]

#create grand total of rows and columns
user_work_count = adorn_totals(user_work_count, where = c("row","col"), fill = "-", na.rm = TRUE, name = "Grand Total")


write.csv(user_work_count, paste("Output/",date_char,"_pivot.CSV",sep = ""), na = "",row.names = FALSE)
write.csv(all_user_data_filt, paste("Output/",date_char,".CSV",sep = ""), na = "",row.names = FALSE)




# user_work_count %>% 
#   kable() %>% 
#   kable_styling(bootstrap_options = "striped", 
#                 full_width = TRUE)

