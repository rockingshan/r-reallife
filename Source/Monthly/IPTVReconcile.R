library(tidyverse)
library(dplyr)
library(readxl)
library(xlsx)
library(stringr)
library(janitor)

smsReport = read_xlsx(file.choose(), skip = 1) #IPTV all ciustomer report
drmReport = read_xlsx(file.choose()) #export STB from DRM
packConfig = read_xlsx(file.choose(), skip = 2) ##pacakagewise Channel details
##clean colnames
packConfig <- janitor::clean_names(packConfig)
smsReport <- janitor::clean_names(smsReport)
drmReport <- janitor::clean_names(drmReport)

packCodes = packConfig %>% select(package_name,plan_code) %>% unique()
colnames(packCodes)[1] = "video_plan"

smsReportMod = merge(smsReport,packCodes,all.x = T)
drmReportMod = drmReport %>% select(cas_serial_number,signature,packages,status)

##Make columns proper
drmReportMod$status <- gsub("a","Active",drmReportMod$status)
drmReportMod$status <- gsub("d","Inactive",drmReportMod$status)
smsReportMod$status <- gsub("Suspended","Inactive",smsReportMod$status)

## make combine column
smsReportMod = smsReportMod %>% unite(combined, c('userid','plan_code','status'),sep = "|",remove = F)
drmReportMod = drmReportMod %>% unite(combined, c('cas_serial_number','packages','status'),sep = "|",remove = F)

finalData = merge(smsReportMod,drmReportMod,by.x = 'userid',by.y = 'cas_serial_number',all.x = T)
nf = finalData[finalData[,4]!= finalData[,10],]  ###find the mismatch between two column data
nf = nf %>% filter(!(is.na(userid)))
write.csv(nf, "Output/IPTV_reconcile.csv",row.names = F)

