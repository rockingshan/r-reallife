library(tidyverse)
library(dplyr)
library(readxl)

inventory = read.csv(file.choose(new = F),colClasses = c(SERIAL_NUMBER="character"))
inventory_select = select(inventory, SERIAL_NUMBER,ITEM_CODE,ENTITY_CODE)

list_sfw = read.csv(file.choose(new = F),colClasses = c(SubscriptionID="character"))
sfw_vc_mq = inventory_select %>% filter(ITEM_CODE=="SAFEVIEWSC")
sfw_vc_mq = separate(sfw_vc_mq, SERIAL_NUMBER, into = c("leftval", "rightval"), sep = 10, remove = FALSE)
sfw_vc_mq$leftval = as.numeric(sfw_vc_mq$leftval)
colnames(sfw_vc_mq)[2] = "SMCs"
sfw_cas_final = merge(list_sfw,sfw_vc_mq,all.x = T) %>% select(SERIAL_NUMBER,SubscriptionID)
write.csv(sfw_cas_final,"Output/Sfw_final.csv",row.names = F)

sheets <- excel_sheets(file.choose(new = F))
data_sheets <- sheets[grepl("CASEntitlement", sheets)]
sheet_df <- map_dfr(data_sheets, ~read_excel(file.choose(new = F), sheet = .x, skip = 1), id = .x)
abv_cas_data = filter(sheet_df, STATUS == "Activated") %>% select(SMARTCARDNO,PACKAGEID)
write.csv(abv_cas_data,"Output/ABV_final.csv",row.names = F)
