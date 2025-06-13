library(tidyverse)
library(dplyr)
library(readxl)
library(stringr)
library(lubridate)

##important to generate these files fresh as historical file may have name mismatch

wallet = read.csv(file.choose(),colClasses = c(Unique.Id="character")) ##wallet report for the duration
listActive = read.csv(file.choose()) ##list of active new - old one has no plan type filter
pacakgeDetails = read.csv(file.choose()) #pacakge details report. If name changes occurred the generate new and then generate wallet. 
planServiceDetails = read.csv(file.choose()) #Plan configuration with services
impBroadcaster = c("Zee Entertainment  Enterprises Limited","Sony Pictures Networks India Pvt. Ltd.","JioStar","Discovery Communications India")
  
##service to Broadcaster block\
pack = pacakgeDetails %>% filter(!Broadcaster %in% c("IndiaCast Media Distribution Pvt. Ltd.", "Star India Pvt. Ltd.","Free to AIR"))
service = planServiceDetails %>% select(Service.Code,Service.Name) %>% unique()
serviceBroadcaster = merge(service,pack) %>% select(Service.Name,Broadcaster) %>% unique()
colnames(serviceBroadcaster)[1] = "SERVICE_NAME"

# STEP 1: Get base plan names
base_plan_names <- listActive %>%
  filter(Plan.Type == "Basic") %>%
  distinct(Plan.Name) %>%
  pull(Plan.Name)

# STEP 2: Filter wallet for Alacarte and Bouquets only
wallet_filtered <- wallet %>%
  filter(!is.na(Amount.Debit)) %>%
  filter(!Plan.Details %in% base_plan_names)

# STEP 3: Map Customer to their base plan
customer_baseplan <- listActive %>%
  filter(Plan.Type == "Basic") %>%
  select(Customer.Nbr, Base.Plan.Name = Plan.Name) %>%
  distinct()

wallet_with_baseplan <- merge(wallet_filtered,customer_baseplan,all.x = T)

# STEP 4: Add LCO City info using Entity.Code
entity_city <- listActive %>%
  select(Entity.Code, Lco.City) %>%
  distinct()

wallet_with_city <- merge(wallet_with_baseplan,entity_city,all.x = T)

# STEP 5: Map Broadcaster
wallet_enriched <- wallet_with_city %>%
  left_join(serviceBroadcaster, by = c("Service.Name" = "SERVICE_NAME")) %>%
  mutate(Broadcaster = ifelse(is.na(Broadcaster), "Unknown Broadcaster", Broadcaster)) %>%
  mutate(Base.Plan.Name = ifelse(is.na(Base.Plan.Name), "Bronze basic", Base.Plan.Name))

# STEP 6a: Base Plan-wise Broadcaster Revenue (Pivoted)
baseplan_broadcaster_revenue <- wallet_enriched %>%
  group_by(Base.Plan.Name, Broadcaster) %>%
  summarise(Total.Revenue = sum(Amount.Debit, na.rm = TRUE), .groups = "drop") %>%
  filter(Broadcaster %in% impBroadcaster) %>%
  pivot_wider(
    names_from = Broadcaster,
    values_from = Total.Revenue,
    values_fill = 0
  )

# STEP 6b: City-wise, Base Plan-wise Broadcaster Revenue (Pivoted)
city_baseplan_broadcaster_revenue <- wallet_enriched %>%
  group_by(Lco.City, Base.Plan.Name, Broadcaster) %>%
  summarise(Total.Revenue = sum(Amount.Debit, na.rm = TRUE), .groups = "drop") %>%
  filter(Broadcaster %in% impBroadcaster) %>%
  pivot_wider(
    names_from = Broadcaster,
    values_from = Total.Revenue,
    values_fill = 0
  )

# VIEW OUTPUTS
print("Base Plan-wise Broadcaster Revenue:")
print(baseplan_broadcaster_revenue)

print("City-wise Base Plan-wise Broadcaster Revenue:")
print(city_baseplan_broadcaster_revenue)

# Optionally export to CSV
write.csv(baseplan_broadcaster_revenue, "baseplan_broadcaster_revenue.csv", row.names = FALSE)
write.csv(city_baseplan_broadcaster_revenue, "city_baseplan_broadcaster_revenue.csv", row.names = FALSE)
