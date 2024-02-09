library(tidyverse)
library(dplyr)
library(readxl)
library(purrr)

msr_details = read.csv(file.choose(new = F),) #import MQ data bouquet
#msr_od = subset(msr_details, (grepl('^MDOR',msr_details$Lco.Code,ignore.case = T)))
#msr_wb = subset(msr_details, !(grepl('^MDOR',msr_details$Lco.Code,ignore.case = T)))

cas_list = msr_details %>% select(Provisioning.System) %>% distinct()
cas_list = cas_list[['Provisioning.System']]
#nagra_code = read.csv(file.choose())

for (casname in cas_list) {
  if(casname != "Nagra"){
    assign(paste0("msr_details_",casname, sep = ""), filter(msr_details, Provisioning.System==casname) %>% select(Cust.Id,Smart.Card.Number,Bouquet_Channel,Service.Cas.Code) %>% unique())
  }else{
    assign(paste0("msr_details_",casname, sep = ""), filter(msr_details, Provisioning.System==casname) %>% select(Cust.Id,Set.Top.Box,Bouquet_Channel,Service.Cas.Code) %>% unique())
  }
  
}
#msr_details_abv_clean = msr_details_ABV[nchar(msr_details_ABV$Smart.Card.Number) != 1, ]
#msr_details_gospell_clean = msr_details_GOSPELL[nchar(msr_details_GOSPELL$Smart.Card.Number) != 1, ]
#msr_details_safeview_clean = msr_details_SAFEVIEW[nchar(msr_details_SAFEVIEW$Smart.Card.Number) != 1, ]
#msr_details_nagra_clean = msr_details_Nagra[nchar(msr_details_Nagra$Set.top.Box) != 1, ]

msr_details_ABV_count = msr_details_ABV %>% group_by(Service.Cas.Code) %>% summarise(Count = n())
msr_details_GOSPELL_count = msr_details_GOSPELL %>% group_by(Service.Cas.Code) %>% summarise(Count = n())
msr_details_SAFEVIEW_count = msr_details_SAFEVIEW %>% group_by(Service.Cas.Code) %>% summarise(Count = n())

#nagra_break = merge(msr_details_Nagra,nagra_code,all.x = T)

#nagra_break$Break.Code[is.na(nagra_break$Break.Code)] <- nagra_break$Service.Cas.Code[is.na(nagra_break$Break.Code)]
#msr_details_Nagra_count = nagra_break %>% group_by(Break.Code) %>% summarise(Count = n())

write.csv(msr_details_ABV_count,"Output/msr_all_ABV_count.csv",row.names = F)
write.csv(msr_details_GOSPELL_count,"Output/msr_all_GOSPELL_count.csv",row.names = F)
write.csv(msr_details_SAFEVIEW_count,"Output/msr_all_SAFEVIEW_count.csv",row.names = F)
#write.csv(msr_details_Nagra_count,"Output/msr_all_Nagra_count.csv",row.names = F)
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

df = msr_details %>% select(Cust.Id,Smart.Card.Number,Broadcaster.Name,Plan.Name,Bouquet_Channel) %>% unique() %>%
  group_by(Broadcaster.Name,Plan.Name,Bouquet_Channel) %>% summarise(Count = n())
write.csv(df,"df.csv",row.names = F)


####nagra cas data breakup####
nagra_data = read.csv(file.choose(),header = F)
nagra_code = read.csv(file.choose())
nagra_data = nagra_data %>% filter(V5 == "MICBSPL")
nagra_break = merge(nagra_data,nagra_code,by.x = 'V1',by.y = 'Service.Cas.Code', all.x = T)
nagra_break$Break.Code[is.na(nagra_break$Break.Code)] <- nagra_break$V1[is.na(nagra_break$Break.Code)]
Nagra_count = nagra_break %>% group_by(Break.Code) %>% summarise(Count = n())
write.csv(Nagra_count,"Output/CAS_Nagra_count.csv",row.names = F)
