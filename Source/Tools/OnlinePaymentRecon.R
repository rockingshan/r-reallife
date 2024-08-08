library(tidyverse)
library(dplyr)

#include HDFC
hdfc = read.csv(file.choose())
hdfc_fl = hdfc %>% select(`Order.No`,`CCAvenue.Ref.`,`Gross.Amount`,`Merchant.Param1`,`Merchant.Param2`,`Payment.Mode`)

# include Razorpay
# razorPay = read.csv(file.choose())
# razorPay_fl = razorPay %>% filter(status == 'captured') %>% select(id,amount,order_id,email,contact,notes)

##Get CCavenue missed payments####
mqAllOnline = read.csv(file.choose()) ##enter Mq All Online Payment Report
mqHdfc = merge(mqAllOnline,hdfc_fl,by.x = 'TRACK_ID',by.y='Order.No',all.x = T)
mqHdfcPending = mqHdfc %>% filter (GATEWAY_NAME == "CCAvenue PG") %>% filter(!(is.na(Merchant.Param1))) %>% filter(STATUS != 'Sucess')
write.csv(mqHdfcPending,"Output/SuccesfulinCCAvenuePendingMQ.csv")

##compare Both gateway with payments entered in system
mqAllPayments = read.csv(file.choose(),colClasses = c(Receipt.="character"))  ##Enter MQ payments report
#Create a new column 'Reference_ID' to store the found reference IDs
# mqAllPayments$Reference_ID <- apply(mqAllPayments, 1, function(row) {
#   ref_id <- NA
#   for (col in c("Receipt.", "Transaction.Number", "NOTES_18")) {
#     if (startsWith(row[[col]], "pay_")) {
#       ref_id <- row[[col]]
#       break
#     }
#   }
#   return(ref_id)
# })
# # Filter the dataframe for Razorpay payments'
# razorPay_mqAllPayments <- mqAllPayments[!is.na(mqAllPayments$Reference_ID), ]
#Filter the df based on HDFC payments
hdfc_mqAllPayments <- mqAllPayments[startsWith(mqAllPayments$Receipt., "1134"), ]

# razorpayMQTally = merge(razorPay_fl,razorPay_mqAllPayments,by.x = 'id',by.y = 'Reference_ID',all.x = T)
# razorpaynotinMQ = razorpayMQTally %>% filter(is.na(Entity.Code))
# write.csv(razorpaynotinMQ,"Output/Razorpay_notin_MQ.csv",row.names = F)

hdfcMQTally = merge(hdfc_fl,hdfc_mqAllPayments,by.x = 'CCAvenue.Ref.',by.y = 'Receipt.',all.x = T)
hdfcnotinMQ = hdfcMQTally %>% filter(is.na(Entity.Code))
write.csv(hdfcnotinMQ,"Output/Hdfc_notin_MQ.csv",row.names = F)


####Find all duplicates ####
# Find duplicate hdfc receipt numbers within the filtered rows
duplicate_receipts <- hdfc_mqAllPayments$Receipt.[duplicated(hdfc_mqAllPayments$Receipt.)]
# Filter the dataframe to include only rows with duplicate hdfc receipt numbers
duplicates_receipts_df <- hdfc_mqAllPayments[hdfc_mqAllPayments$Receipt. %in% duplicate_receipts, ]
# # Find duplicate  razorpay reference IDs
# duplicate_ids <- razorPay_mqAllPayments$Reference_ID[duplicated(razorPay_mqAllPayments$Reference_ID)]
# # Filter the dataframe to include only rows with duplicate reference IDs
# duplicates_df <- razorPay_mqAllPayments[razorPay_mqAllPayments$Reference_ID %in% duplicate_ids, ]
# allDuplicates <- rbind(duplicates_df,duplicates_receipts_df)
write.csv(duplicates_receipts_df,"Output/Duplicate_HDFC_payments.csv",row.names = F)
