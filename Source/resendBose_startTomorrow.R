library(tidyverse)
library(dplyr)
library(lubridate)
library(httr)

# Define directory path
dir_path <- "C:/Users/Shantanu/Downloads"

# List files in directory
files <- list.files(dir_path)

# Use regular expressions to match files ending in "_DUEFORRENEWAL.CSV"
pattern <- "_DUEFORRENEWALS\\.CSV$"
matching_files <- files[grep(pattern, files)]

# Get modification times of filtered files
file_info <- file.info(file.path(dir_path, matching_files))

# Select most recent file
most_recent_file <- matching_files[which.max(file_info$mtime)]

# Read most recent file
custList <- read.csv(file.path(dir_path, most_recent_file))

#custList = read.csv(file.choose())
custList$Smart.Card.Number = gsub("'","",custList$Smart.Card.Number)
custList <- custList %>% mutate(VC.length = nchar(Smart.Card.Number),  .after = 7) # get character length of vc
custList$VC.length <- gsub("8","GOSPELL",custList$VC.length, fixed = TRUE)
custList$VC.length <- gsub("12","SAFEVIEW",custList$VC.length, fixed = TRUE)
custList$VC.length <- gsub("15","NAGRA",custList$VC.length, fixed = TRUE)
custList$VC.length <- gsub("16","ABV",custList$VC.length, fixed = TRUE) #REPLACE LENGTHS TO CAS NAMES
custList$Contract.End.Date = as.Date(custList$Contract.End.Date, "%d/%m/%Y")
custListSlct = custList %>% filter(Entity.Code %in% c("MDCH063")) %>% filter(VC.length %in% c("ABV","GOSPELL")) %>% filter(Contract.End.Date == today()+30)
custListSlct = custListSlct %>% select(Customer.Number,Contract.Number,Smart.Card.Number,VC.length)
# Start timing the execution of the for loop
start_time <- Sys.time()

# Loop over each row of the data frame and make an HTTP request for each customer
for (i in 1:nrow(custListSlct)) {
  mqdate = format(today(), format="%d/%m/%Y")
  # Create the request body for the HTTP request using the customer's account number, mobile number, and type
  body1 <- paste0("<REQUESTINFO>\r\n<KEY_NAMEVALUE>\r\n<KEY_NAME>CONTRACTNO</KEY_NAME>\r\n<KEY_VALUE>",custListSlct[i, "Contract.Number"],"</KEY_VALUE>\r\n</KEY_NAMEVALUE>\r\n<DISCONNECTIONINFO>\r\n<DISCONNECTIONDATE>",mqdate,"</DISCONNECTIONDATE>\r\n<REASON>VACATION</REASON>\r\n<REMARKS>DISCONNECT</REMARKS>\r\n</DISCONNECTIONINFO>\r\n</REQUESTINFO>\r\n")
  
  # Create the headers for the HTTP request
  headers <- c(
    'USERNAME' = 'MB102',
    'PASSWORD' = 'Shan8648',
    'EXTERNALPARTY' = 'MQS',
    'Content-Type' = 'application/xml'
  )
  
  # Generate a random reference number and replace the hardcoded value in the URL with it
  date <- Sys.time()
  ref_no <- paste0(format(date, format = "%d%m%Y%H%M%S"), "ABRTEY")
  url1 <- paste0("https://meghbela-bcrm.magnaquest.com/RestService/RestService.svc/DisconnectContract?referenceno=", ref_no)
  
  # Make the HTTP request using the POST method, the request URL, the request body, and the headers
  res <- VERB("POST", url = url1, body = body1, add_headers(headers))
  
  # Print the response to the console
  cat(content(res, 'text'))
  
  # Pause for 2 min before making the next HTTP request
  Sys.sleep(90)
  
  mqdate = format(today(), format="%d/%m/%Y")
  body2 <- paste0("<REQUESTINFO>\r\n<KEY_NAMEVALUE>\r\n<KEY_NAME>CONTRACTNO</KEY_NAME>\r\n<KEY_VALUE>",custListSlct[i, "Contract.Number"],"</KEY_VALUE>\r\n</KEY_NAMEVALUE>\r\n<RECONNECTIONINFO>\r\n<RECONNECTIONDATE>",mqdate,"</RECONNECTIONDATE>\r\n<REMARKS></REMARKS>\r\n<REASONCODE></REASONCODE>\r\n</RECONNECTIONINFO>\r\n</REQUESTINFO>\r\n")
  
  
  # Generate a random reference number and replace the hardcoded value in the URL with it
  date <- Sys.time()
  ref_no2 <- paste0(format(date, format = "%d%m%Y%H%M%S"), "ABRTEY")
  url2 <- paste0("https://meghbela-bcrm.magnaquest.com/RestService/RestService.svc/ReconnectContract?referenceno=", ref_no2)
  
  # Make the HTTP request using the POST method, the request URL, the request body, and the headers
  res <- VERB("POST", url = url2, body = body2, add_headers(headers))
  
  # Print the response to the console
  cat(content(res, 'text'))
  Sys.sleep(5)
  
}

# End timing the execution of the for loop
end_time <- Sys.time()

# Print the elapsed time taken to execute the for loop
cat("Elapsed time:", end_time - start_time, "\n")
write.csv(custListSlct,paste0(ref_no2,".csv"),row.names = F)
