library(tidyverse)
library(dplyr)
library(lubridate)

activ_cust = read.csv(file.choose(new = F))
active_cust_for_lookup = activ_cust %>% select(Entity.Code,Entity.Name) %>% unique()

wallet = read.csv(file.choose(new = F),colClasses = c(Unique.Id="character"))
#wallet = filter(wallet, !(Entity.Code %in% hdnd_nm))


active_pivot = activ_cust %>% 
  group_by(Entity.Code) %>%
  summarize(Total_Active = sum(Active.Customer))
active_pivot = merge(active_pivot, active_cust_for_lookup)
active_pivot = active_pivot[, c(1,3,2)]
#active_pivot_n = filter(active_pivot, grepl('MSW', Entity.Code,fixed = TRUE))

active_wallet = wallet %>% select(Entity.Code,Unique.Id) %>% unique()
active_wallet <- active_wallet %>% filter(Unique.Id != "" & !is.na(Unique.Id))
active_wallet_count = active_wallet %>% group_by(Entity.Code) %>% summarise(WallCount = n())

active_piv_wall = merge(active_pivot,active_wallet_count,all.x = T)
# Replace NA with 0
active_piv_wall <- active_piv_wall %>% mutate(across(everything(), ~ replace_na(., 0)))

# Create a new column with the max value of A and B
active_piv_wall <- active_piv_wall %>%
  mutate(ActiveCustomer = pmax(Total_Active, WallCount))

active_piv_wall_cl = active_piv_wall %>% select(Entity.Code,Entity.Name,ActiveCustomer)

write.csv(active_piv_wall_cl, sprintf("Output/LCOWISE_ACTIVE_CUST_%s_%g.csv",month(today() - months(1),label = TRUE, abbr = F),year(today())), row.names = FALSE)

#if current month is January then run following block
#write.csv(active_pivot, sprintf("Output/LCOWISE_ACTIVE_CUST_%s_%g.csv",month(today() - months(1),label = TRUE, abbr = F),year(rollback(today()))), row.names = FALSE)

#the month command pastes previous month
