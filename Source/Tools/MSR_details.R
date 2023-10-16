library(tidyverse)
library(dplyr)
library(readxl)
library(purrr)

list_bouquet_dated = read.csv(file.choose(new = F),colClasses = c(Service.Cas.Code="character")) #import MQ data bouquet
list_alacarte = read.csv(file.choose(new = F),colClasses = c(Service.Cas.Code="character")) #import MQ alacarte details

# list_active = read.csv(file.choose(new = F))
# list_bq_selct = list_bouquet_dated %>% select(Cust.Id,Set.Top.Box,Smart.Card.Number,Plan.Name) %>% unique()
# list_bq_selct = list_bq_selct %>% unite(combined, c("Cust.Id", "Plan.Name"))
# list_act_bq = list_active %>% select(CUSTOMER_NBR,ENTITY_CODE,ENTITY_NAME,PLAN_NAME) %>% unique()
# list_act_bq = list_act_bq %>% unite(combined, c("CUSTOMER_NBR", "PLAN_NAME"))
# bouquet_to_active = merge(list_bq_selct,list_act_bq,all.x = T,all.y = F)
# write.csv(bouquet_to_active,"bouquet_report_to_active.csv",row.names = F)

colnames(list_bouquet_dated)[5] <- "Bouquet_Channel"
colnames(list_bouquet_dated)[6] <- "Broadcaster.Name"
colnames(list_alacarte)[5] <- "Bouquet_Channel"
list_bouq_al = rbind(list_bouquet_dated,list_alacarte)
##delete zee media
list_bouq_al_1 = list_bouq_al[list_bouq_al$Broadcaster.Name != "Zee Media Corporation Ltd", ]
##delete wm channels
# Define a vector of values to check against
channels_to_remove <- c("Pogo", "Turner Kids Pack", "Cartoon Network","CNN International","Turner Family Pack")
list_bouq_al_2 = list_bouq_al_1[!(list_bouq_al_1$Broadcaster.Name == "Discovery Communications India" & list_bouq_al_1$Bouquet_Channel %in% channels_to_remove), ]
write.csv(list_bouq_al_2,"MSR_detailed_report_07122022.csv",row.names = F)
