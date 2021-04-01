library(tidyverse)
library(dplyr)

activ_cust = read.csv(choose.files(default = "_StatewiseActiveCustomerReport.CSV",caption = "Select Active Customer File",multi = FALSE,))
active_cust_for_lookup = activ_cust %>% select(Entity.Code,Entity.Name) %>% unique()

active_pivot = activ_cust %>% 
  group_by(Entity.Code) %>%
  summarize(Total_Active = sum(Active.Customer))
active_pivot = merge(active_pivot, active_cust_for_lookup)
active_pivot = active_pivot[, c(1,3,2)]
#active_pivot_n = filter(active_pivot, grepl('MSW', Entity.Code,fixed = TRUE))


write.csv(active_pivot, "LCOWISE_ACTIVE_CUST.csv", row.names = FALSE)
#write.csv(active_pivot_n, "BPC_LCO_DATA.csv", row.names = FALSE)
