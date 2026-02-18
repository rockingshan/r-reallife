library(tidyverse)
library(dplyr)
library(readxl)
library(openxlsx)
source('Source/Functions.r')

##opens a window to select files,
singlepack_7 = read.csv(file.choose())
pack = read.csv(file.choose()) %>%
  select(Service.Code, Channel, Broadcaster) %>%
  unique()
pack = pack %>%
  filter(
    !Broadcaster %in%
      c("IndiaCast Media Distribution Pvt. Ltd.", "Star India Pvt. Ltd.")
  )
service = read.csv(file.choose()) %>%
  select(Service.Code, Service.Name) %>%
  unique()
serviceChannel = merge(service, pack) %>%
  select(Service.Name, Channel) %>%
  unique()
colnames(serviceChannel)[1] = "Service.Name"
bundle_services = read.csv(file.choose())

list_active = read.csv(choose.files())
##new list of active customer report
list_active_base = list_active %>% filter(Plan.Type == "Basic")
list_active_addon_ala = list_active %>%
  filter(Plan.Type %in% c("Addon", "Alacarte"))
bouquet_names = read.csv(file.choose()) # special if need add manually
bouquet_names = bouquet_names %>% add_row(Bouquet = "Bronze Basic")

list_active_base_filter = list_active_base %>%
  group_by(Plan.Name, Service.Name) %>%
  select(Customer.Nbr, Service.Name, Plan.Name, ) %>%
  unique() %>%
  summarize(Active_count = n())
list_active_base_filter_spread = merge(list_active_base_filter, singlepack_7)
list_active_base_dpo = list_active_base_filter_spread %>% filter(X == "Bouquet")
list_active_base_dpo_merged = merge(
  list_active_base_dpo,
  serviceChannel,
  by.x = "Bouquet",
  by.y = "Service.Name"
) %>%
  select(Plan.Name, Bouquet, Channel, Active_count)
list_active_base_dpo_merged = add_column(
  list_active_base_dpo_merged,
  PackType = 'DPO Pack with broadcaster bouquets',
  .after = 2
)

list_active_base_dpo_al = list_active_base_filter_spread %>%
  filter(X == "Alacarte") %>%
  select(Plan.Name, Bouquet, Active_count)
list_active_base_dpo_al = add_column(
  list_active_base_dpo_al,
  PackType = 'DPO Pack with Alacarte',
  .after = 2
)
list_active_base_dpo_al = list_active_base_dpo_al %>%
  mutate(Channel = Bouquet, .after = 3)

##bouquets
list_active_addon_ala_filter = list_active_addon_ala %>%
  group_by(Plan.Name, Service.Name) %>%
  select(Customer.Nbr, Service.Name, Plan.Name, ) %>%
  unique() %>%
  summarize(Active_count = n())
list_active_addon_ala_merged = merge(
  list_active_addon_ala_filter,
  serviceChannel,
  by.x = "Service.Name",
  by.y = "Service.Name"
) %>%
  select(Plan.Name, Service.Name, Channel, Active_count)

list_active_addon = list_active_addon_ala_merged %>%
  filter(!(Plan.Name %in% bundle_services$Plan.Name))
list_active_addon = add_column(
  list_active_addon,
  PackType = 'Broadcaster Bouqets',
  .after = 2
)
colnames(list_active_addon)[2] <- "Bouquet"
##alacarte
list_active_ala = list_active_addon_ala_merged %>%
  filter((Plan.Name %in% bundle_services$Plan.Name))
list_active_ala = add_column(
  list_active_ala,
  PackType = 'Alacarte',
  .after = 2
)
colnames(list_active_ala)[2] <- "Bouquet"

##final
list_active_final = rbind(
  list_active_base_dpo_merged,
  list_active_base_dpo_al,
  list_active_addon,
  list_active_ala
)

list_active_final_pivot = list_active_final %>%
  group_by(Channel, PackType) %>%
  summarize(TotalSubs = sum(Active_count)) %>%
  pivot_wider(names_from = PackType, values_from = TotalSubs)
list_active_final_pivot = list_active_final_pivot %>%
  mutate(Sum = rowSums(across(where(is.numeric)), na.rm = TRUE))
write.csv(list_active_final_pivot, "Channel_penetration_count.csv")
