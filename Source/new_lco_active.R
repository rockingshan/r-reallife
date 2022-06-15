library(tidyverse)
library(dplyr)
library(lubridate)

date_char = format(Sys.Date()-1, format="%d%m%Y") #previous date 
inventory = read.csv(file.choose(new = F))

lco_det = read.csv(file.choose(new = F)) #lco master file
lco_det$Lco.Code <- gsub("'","",lco_det$Lco.Code)
lco_det <- lco_det %>% select(Lco.Code,Business.Name)
colnames(lco_det)[1] <- "Entity.Code"  #change column name
activ_cust = read.csv(file.choose(new = F)) #StateWiseActiveCust previous date
activ_cust = activ_cust %>% filter(!(Entity.Code == 'MD0305')) ###remove pragati

  
##pragati 2 special - calculate from list of active
pra_two = read.csv(file.choose(new = F)) #3list of active pragati two only
pra_two$STB <- gsub("'","",pra_two$STB)
pra_two$SC <- gsub("'","",pra_two$SC)
colnames(pra_two)[10] <- "VC"
colnames(pra_two)[11] <- "STB"
pra_two = pra_two %>% select(CUSTOMER_NBR,ENTITY_CODE,VC) %>% unique()
pra_two_new = merge(pra_two,pragati_vc,all.x = F,all.y = F)
pra_two_pivot =  pra_two_new %>% group_by(ENTITY_CODE) %>% summarise(Active = n())

new_lco = read.csv("C:/Docu__/r-reallife/Data/lcolist.txt")
pragati_vc = read.csv("C:/Docu__/r-reallife/Data/PRAGATI.csv",colClasses = c("character"))

active_pivot = activ_cust %>% 
  group_by(Entity.Code) %>%
  summarize(Total_Active = sum(Active.Customer))
##extract a single element from dataframe and add to existing dataframe
active_pivot = active_pivot %>% add_row(Entity.Code = 'MD0305', Total_Active = as.integer(pra_two_pivot[1,"Active"]) )

inv_slt = inventory %>% filter(!(str_detect(ITEM_CODE, "SC"))) %>%
  select(SERIAL_NUMBER,TYPE,ITEM_DESCR,LOCATION_DESCR,ENTITY_CODE,CUSTOMER_NBR) %>% unique()
inv_pivot = inv_slt %>% group_by(ENTITY_CODE) %>% summarise(Total_STB = n())
colnames(inv_pivot)[1] <- 'Entity.Code'
inv_pivot = inv_pivot %>% filter(!(Entity.Code == 'MD0305'))
inv_pivot = inv_pivot %>% add_row(Entity.Code = 'MD0305', Total_STB = 200 )
inv_pivot = merge(inv_pivot, lco_det)
inv_pivot = inv_pivot[, c(1,3,2)]  ##reorder columns

lco_active = merge(active_pivot,inv_pivot, all.x = F,all.y = T)
lco_active_new = merge(new_lco,lco_active,all.x = T)
lco_active_new = lco_active_new[, c(1,3,2,4)]



write.csv(lco_active_new,paste("Output/New_LCO_data_",date_char,".CSV",sep = ""), na = "",row.names = FALSE)
