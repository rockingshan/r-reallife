library(tidyverse)
library(here)
library(dplyr)
library(readxl)
library(skimr)
library(kableExtra)

bc_odisha = read_xlsx(here("data/4109450_STATE_BOUQUET_RPT_ODISHA_NOV20.XLS"),skip = 3,)
#skimr::skim(bc_odisha)
names(bc_odisha) = make.names(names(bc_odisha))
bc_odisha = bc_odisha[!grepl("Plan Name", bc_odisha$Plan.Name),]
bc_odisha = bc_odisha[!grepl("Broadcaster Name*", bc_odisha$Month),]

bc_odisha_filterred = bc_odisha %>%
  select(Broadcaster.Name,Plan.Name,Bouquet,Monthly.Subs.of.the.Channel) %>% distinct()%>% na.omit()
bc_name = bc_odisha %>%
  select(Broadcaster.Name,Bouquet) %>% distinct()%>% na.omit()

bc_odisha_filterred$Monthly.Subs.of.the.Channel = as.numeric(bc_odisha_filterred$Monthly.Subs.of.the.Channel)

active_pivot = bc_odisha_filterred %>% 
  group_by(Bouquet) %>%
  summarize(Total_Active = sum(Monthly.Subs.of.the.Channel))

active_pivot = merge(bc_name,active_pivot)
write.csv(bc_odisha_filterred, "ODISHA_bc.csv", row.names = FALSE)