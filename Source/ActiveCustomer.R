library(tidyverse)
library(dplyr)
library(lubridate)

activ_cust = read.csv(file.choose(new = F))
active_cust_for_lookup = activ_cust %>% select(Entity.Code,Entity.Name) %>% unique()

active_pivot = activ_cust %>% 
  group_by(Entity.Code) %>%
  summarize(Total_Active = sum(Active.Customer))
active_pivot = merge(active_pivot, active_cust_for_lookup)
active_pivot = active_pivot[, c(1,3,2)]
#active_pivot_n = filter(active_pivot, grepl('MSW', Entity.Code,fixed = TRUE))


write.csv(active_pivot, sprintf("Output/LCOWISE_ACTIVE_CUST_%s_%g.csv",month(today() - months(1),label = TRUE, abbr = F),year(today())), row.names = FALSE)

#if current month is January then run following block
#write.csv(active_pivot, sprintf("Output/LCOWISE_ACTIVE_CUST_%s_%g.csv",month(today() - months(1),label = TRUE, abbr = F),year(rollback(today()))), row.names = FALSE)

#the month command pastes previous month
