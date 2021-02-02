library(tidyverse)
library(here)
library(dplyr)
library(readxl)
library(stringr)
library(knitr)
library(kableExtra)
library(janitor)

date_char = format(Sys.Date()-1, format="%d%m%Y") #previous date 

#create list from the files 
file.list = list.files(path = "F:/drive2/Daily report/", pattern = paste(date_char,"*",sep = ""), all.files = FALSE, full.names = TRUE) #find file of yesterday
file.list = list.files(path = "C:/Users/SHANTANU/Desktop/JAN/", pattern = NULL, all.files = FALSE, full.names = TRUE) #find file all month
#create list from file names
df.list <- lapply(file.list, read_excel)

#merge files in one variable and clear names
all_user_data <- bind_rows(df.list, .id = "id")
names(all_user_data) = make.names(names(all_user_data))
all_user_data$Date = as.character(all_user_data$Date) #convert to charracter
class(all_user_data$Date)
all_user_data1 = all_user_data[!grepl("*00:02:04",all_user_data$Date,fixed = TRUE),]
#all_user_data = all_user_data %>% select(Customer..,Transaction.type,User.ID)

#remove rows where account number is null for one time sale
all_user_data = all_user_data[!(is.na(all_user_data$Customer..) & all_user_data$Transaction.type=="One Time Sale"),]

#filter file with based on criteria
target = c("Add Contract","Add Credit Note","Create Customer","Disconnect Contract","HARDWARE CHANGE","Hardware Return","Modify Contract","One Time Sale","Reconnect Contract","RETRACK","New Payment","ChangePwd")
all_user_data_filt = filter(all_user_data, Transaction.type %in% target) %>% distinct()

#make pivot table and order by user
user_work_count = all_user_data_filt %>% group_by(User.ID,Transaction.type) %>% summarize(Transaction_count = n()) %>% 
  pivot_wider(names_from = User.ID, values_from = Transaction_count)

#create grand total of rows and columns
user_work_count = adorn_totals(user_work_count, where = c("row","col"), fill = "-", na.rm = TRUE, name = "Grand Total")

write.csv(user_work_count, paste("F:/drive2/Daily report/",date_char,"_ALL_USER.CSV",sep = ""), row.names = FALSE).


# user_work_count %>% 
#   kable() %>% 
#   kable_styling(bootstrap_options = "striped", 
#                 full_width = TRUE)

