library(tidyverse)
library(dplyr)
library(lubridate)

activ_cust = read.csv(file.choose(new = F))

df_base = activ_cust %>% filter(Plan.Type == 'Basic')
df_addon = activ_cust %>% filter(Plan.Type == 'Addon')

df_addon_count = df_addon %>% group_by(Plan.Name) %>%
  summarise(Active.Count = n())
write.csv(df_addon_count,"Addon_count.csv", row.names=F)
