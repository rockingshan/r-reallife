library(tidyverse)
library(dplyr)
library(readxl)
library(purrr)

msr_details = read.csv(file.choose(new = F),) #import MQ data bouquet
msr_od = subset(msr_details, (grepl('^MDOR',msr_details$Lco.Code,ignore.case = T)))
msr_wb = subset(msr_details, !(grepl('^MDOR',msr_details$Lco.Code,ignore.case = T)))

cas_list = msr_od %>% select(Provisioning.System) %>% distinct()
cas_list = cas_list[['Provisioning.System']]

for (casname in cas_list) {
  if(casname != "Nagra"){
    assign(paste0("msr_wb_",casname, sep = ""), filter(msr_wb, Provisioning.System==casname) %>% select(Cust.Id,Smart.Card.Number,Bouquet_Channel,Service.Cas.Code) %>% unique())
  }else{
    assign(paste0("msr_wb_",casname, sep = ""), filter(msr_wb, Provisioning.System==casname) %>% select(Cust.Id,Set.Top.Box,Bouquet_Channel,Service.Cas.Code) %>% unique())
  }
  
}
#msr_wb_abv_clean = msr_wb_ABV[nchar(msr_wb_ABV$Smart.Card.Number) != 1, ]
#msr_wb_gospell_clean = msr_wb_GOSPELL[nchar(msr_wb_GOSPELL$Smart.Card.Number) != 1, ]
#msr_wb_safeview_clean = msr_wb_SAFEVIEW[nchar(msr_wb_SAFEVIEW$Smart.Card.Number) != 1, ]
#msr_wb_nagra_clean = msr_wb_Nagra[nchar(msr_wb_Nagra$Set.top.Box) != 1, ]

msr_wb_ABV_count = msr_wb_ABV %>% group_by(Service.Cas.Code) %>% summarise(Count = n())
msr_wb_GOSPELL_count = msr_wb_GOSPELL %>% group_by(Service.Cas.Code) %>% summarise(Count = n())
msr_wb_SAFEVIEW_count = msr_wb_SAFEVIEW %>% group_by(Service.Cas.Code) %>% summarise(Count = n())
#msr_wb_Nagra_count = msr_wb_Nagra %>% group_by(Service.Cas.Code) %>% summarise(Count = n())

write.csv(msr_wb_ABV_count,"Output/msr_wb_ABV_count.csv",row.names = F)
write.csv(msr_wb_GOSPELL_count,"Output/msr_wb_GOSPELL_count.csv",row.names = F)
write.csv(msr_wb_SAFEVIEW_count,"Output/msr_wb_SAFEVIEW_count.csv",row.names = F)
#write.csv(msr_wb_ABV_count,"Output/msr_wb_ABV_count.csv",row.names = F)
######ODISHA####
cas_list = msr_od %>% select(Provisioning.System) %>% distinct()
cas_list = cas_list[['Provisioning.System']]

for (casname in cas_list) {
  if(casname != "Nagra"){
    assign(paste0("msr_od_",casname, sep = ""), filter(msr_od, Provisioning.System==casname) %>% select(Cust.Id,Smart.Card.Number,Bouquet_Channel,Service.Cas.Code) %>% unique())
  }
}
#msr_od_abv_clean = msr_od_ABV[nchar(msr_od_ABV$Smart.Card.Number) != 1, ]
#msr_od_gospell_clean = msr_od_GOSPELL[nchar(msr_od_GOSPELL$Smart.Card.Number) != 1, ]
#msr_od_safeview_clean = msr_od_SAFEVIEW[nchar(msr_od_SAFEVIEW$Smart.Card.Number) != 1, ]


msr_od_ABV_count = msr_od_ABV %>% group_by(Service.Cas.Code) %>% summarise(Count = n())
msr_od_GOSPELL_count = msr_od_GOSPELL %>% group_by(Service.Cas.Code) %>% summarise(Count = n())
msr_od_SAFEVIEW_count = msr_od_SAFEVIEW %>% group_by(Service.Cas.Code) %>% summarise(Count = n())


write.csv(msr_od_ABV_count,"Output/msr_od_ABV_count.csv",row.names = F)
write.csv(msr_od_GOSPELL_count,"Output/msr_od_GOSPELL_count.csv",row.names = F)
write.csv(msr_od_SAFEVIEW_count,"Output/msr_od_SAFEVIEW_count.csv",row.names = F)
