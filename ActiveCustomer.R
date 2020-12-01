library(tidyverse)
library(here)
library(dplyr)

activ_cust = read.csv(here("data/4038273_StatewiseActiveCustomerReport.CSV"))
active_cust_for_lookup = activ_cust %>% select(Entity.Code,Entity.Name) %>% unique()

active_pivot = activ_cust %>% 
  group_by(Entity.Code) %>%
  summarize(Total_Active = sum(Active.Customer))
active_pivot = merge(active_pivot, active_cust_for_lookup)
active_pivot = active_pivot[, c(1,3,2)]

write.csv(active_pivot, "ACTIVE_CUSTOMER_NOV20.csv", row.names = FALSE)
