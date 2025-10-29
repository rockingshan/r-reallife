library(tidyverse)
library(dplyr)
inventory = read.csv(file.choose(new = F),colClasses = c(SERIAL_NUMBER="character")) ##inventory file
customer_data = read.csv(file.choose(new = F)) ##customer master data
#lcoarea = read.csv(file.choose(new = F))
####work on all area inventory file

cardtype = c('Smart Card','ABV Smart Card','Sumavision SC','Gospell SC','Safeview SC','Nagra Cardless STB','Nagra 2027N','Conax SC')
inventory_all = inventory %>% filter(ITEM_DESCR %in% cardtype) %>% 
  select(SERIAL_NUMBER,TYPE,ITEM_CODE,ITEM_DESCR,LOCATION_DESCR,ENTITY_CODE,CUSTOMER_NBR) %>% unique()
inventory_all = inventory_all %>% mutate(ITEM_DESCR = case_when(
  ITEM_DESCR == "ABV Smart Card" ~ "ABV",
  ITEM_DESCR == "Gospell SC" ~ "GOSPELL",
  ITEM_DESCR == "Sumavision SC" ~ "SUMAVISION",
  ITEM_DESCR == "Nagra Cardless STB" ~ "NAGRA",
  ITEM_DESCR == "Safeview SC" ~ "SAFEVIEW",
  ITEM_DESCR == "Conax SC" ~ "CONAX",
  ITEM_DESCR == "Nagra 2027N" ~ "NAGRA",
  TRUE ~ ITEM_DESCR  # default fallback
))
inventory_all_trimmed = inventory_all %>% select(SERIAL_NUMBER,ITEM_DESCR) %>% unique()
colnames(inventory_all_trimmed)[2] <- "PROV_SYSTEM"
customer_data$Stb <- gsub("'","",customer_data$Stb, fixed = TRUE)
customer_data$Sc <- gsub("'","",customer_data$Sc, fixed = TRUE)

customer_data_cas = merge(customer_data,inventory_all_trimmed, by.x = 'Sc',by.y = 'SERIAL_NUMBER')
write.csv(customer_data_cas,"Customer_Active_inactive_with_CASNAME_09072025.csv",row.names = F)
