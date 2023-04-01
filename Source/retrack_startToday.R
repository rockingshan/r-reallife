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
custListSlct = custList %>% filter(VC.length %in% c("ABV","GOSPELL")) %>% filter(Contract.End.Date == today()+29)
custListSlct = custListSlct %>% select(Customer.Number,Smart.Card.Number,VC.length)
# Start timing the execution of the for loop
start_time <- Sys.time()
# Loop over each row of the data frame and make an HTTP request for each customer
for (i in 1:nrow(custListSlct)) {
  # Create the request body for the HTTP request using the customer's account number, mobile number, and type
  body <- paste0("<REQUESTINFO>\r\n<RETRACK>\r\n<CUSTOMERNUMBER>", custListSlct[i, "Customer.Number"], "</CUSTOMERNUMBER>\r\n<HARDWAREREFNUMBER>", custListSlct[i, "Smart.Card.Number"], "</HARDWAREREFNUMBER>\r\n<TYPE>P</TYPE>\r\n<PROVISIONINGSYSTEMCODE>", custListSlct[i, "VC.length"], "</PROVISIONINGSYSTEMCODE>\r\n<INITIALIZE>Y</INITIALIZE>\r\n<RETRACKALL>Y</RETRACKALL>\r\n</RETRACK>\r\n</REQUESTINFO>")
  
  # Create the headers for the HTTP request
  headers <- c(
    'USERNAME' = 'MB102',
    'PASSWORD' = 'Shan4935',
    'EXTERNALPARTY' = 'MQS',
    'Content-Type' = 'application/xml'
  )
  
  # Generate a random reference number and replace the hardcoded value in the URL with it
  date <- Sys.time()
  ref_no <- paste0(format(date, format = "%d%m%Y%H%M%S"), "ABRTEY")
  url <- paste0("https://meghbela-bcrm.magnaquest.com/RestService/RestService.svc/Retrack?referenceno=", ref_no)
  
  # Make the HTTP request using the POST method, the request URL, the request body, and the headers
  res <- VERB("POST", url = url, body = body, add_headers(headers))
  
  # Print the response to the console
  #cat(content(res, 'text'))
  
  # Pause for 5 seconds before making the next HTTP request
  #;Sys.sleep(.3)
  cat(i)
  cat("/")
}

# End timing the execution of the for loop
end_time <- Sys.time()

# Print the elapsed time taken to execute the for loop
cat("Elapsed time:", end_time - start_time, "\n")