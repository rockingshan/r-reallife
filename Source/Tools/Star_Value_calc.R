library(dplyr)
library(readr)
library(openxlsx)
library(knitr)
library(plotly)
library(tidyr)
library(ggplot2)
# Analytical Report Script for STAR
# This script performs analytical operations on channel plan data.
# Sections are modular and can be run independently.


list_active = read.csv(file.choose())
list_active1 = list_active %>% filter(!(PLAN_CODE == '')) %>% select(CUSTOMER_NBR,CONTRACT_NUMBER,ENTITY_CODE,LCO_CITY,STB,SERVICE_CODE,SERVICE_NAME,PLAN_CODE,PLAN_NAME) %>%
  filter(!(PLAN_NAME == 'DPO Promotional Bundle')) %>% filter(!(PLAN_NAME == 'zDelete Pairing')) %>% filter(!(PLAN_NAME == 'All Channel Special Pack')) %>% unique()
pack_details = read.csv(file.choose()) %>% select(Service.Code,Broadcaster) %>% unique()
colnames(pack_details)[1] = "SERVICE_CODE"
colnames(pack_details)[2] = "Broadcaster.Name"
listActiveBroadcast = merge(list_active1,pack_details,by = "SERVICE_CODE", all.x = T) %>% filter(!(is.na(Broadcaster.Name)))
listActiveBroadcastmod <- listActiveBroadcast %>%
  mutate(PLAN_TYPE = case_when(
    startsWith(PLAN_CODE, "MBIL") ~ "BasePlan",
    startsWith(PLAN_CODE, "ALACARTE")  ~ "Alacarte",
    PLAN_CODE == "FTAPLAN"     ~ "BasePlan",
    PLAN_CODE == "DDPLAN"     ~ "BasePlan",
    TRUE ~ "AddonPlan"  # default if none of the conditions match
  ))
broadcaster_name = "Discovery Communications India"

# --- [1. Unique Customer Count per Base Plan] ---
baseplan_data <- listActiveBroadcastmod %>%
  filter(PLAN_TYPE == "BasePlan") %>%
  distinct(CUSTOMER_NBR, PLAN_NAME, LCO_CITY)

baseplan_summary <- baseplan_data %>%
  group_by(LCO_CITY, PLAN_NAME) %>%
  summarise(Plan_Customers = n(), .groups = "drop") %>%
  group_by(LCO_CITY) %>%
  mutate(
    Total_Customers_City = sum(Plan_Customers),
    Percentage = round((Plan_Customers / Total_Customers_City) * 100, 2)
  ) %>%
  arrange(PLAN_NAME, desc(Plan_Customers))

# Save to Excel and Markdown
wb <- createWorkbook()
addWorksheet(wb, "BasePlan_Unique_Customer_Count")
writeData(wb, sheet = "BasePlan_Unique_Customer_Count", baseplan_summary)
saveWorkbook(wb, paste0(broadcaster_name,"_analytical_report.xlsx"), overwrite = TRUE)


# --- [2. JioStar Alacarte/Addon Valuation Analysis] ---
# You must prepare a CSV with SERVICE_CODE and PRICE for JioStar services
price_data <- read_csv(file.choose())  # Must contain columns: SERVICE_CODE, PRICE

# Filter Alacarte and Addon from JioStar
others_data <- listActiveBroadcastmod %>%
  filter(PLAN_TYPE %in% c("Alacarte", "AddonPlan"), `Broadcaster.Name` == broadcaster_name) ##change broadcaster

# Merge price
others_priced <- merge(others_data,price_data,by = "SERVICE_CODE", all.x = T)

# Map BasePlan per CUSTOMER_NBR
baseplan_map <- baseplan_data %>%
  group_by(CUSTOMER_NBR) %>%
  slice(1)  # If multiple baseplans per customer, take the first one

others_with_base <- others_priced %>%
  left_join(baseplan_map, by = "CUSTOMER_NBR", suffix = c("", "_BASE"))

# Calculate total value and bucket it
customer_value <- others_with_base %>%
  group_by(CUSTOMER_NBR, LCO_CITY, PLAN_NAME_BASE) %>%
  summarise(Total_Value = sum(PRICE, na.rm = TRUE), .groups = 'drop') %>%
  mutate(Value_Bucket = case_when(
    Total_Value < 30 ~ "Less_than_30",
    Total_Value >= 30 & Total_Value <= 55 ~ "30_to_55",
    Total_Value > 55 ~ "Above_55",
    TRUE ~ "Other"  # Fallback category (optional)
  ))

# Aggregate count
city_plan_summary <- customer_value %>%
  # Remove rows where PLAN_NAME_BASE is NA
  filter(!is.na(PLAN_NAME_BASE)) %>%
  group_by(LCO_CITY, PLAN_NAME_BASE, Value_Bucket) %>%
  summarise(Customer_Count = n(), .groups = 'drop') %>%
  pivot_wider(
    names_from = Value_Bucket,
    values_from = Customer_Count,
    names_prefix = "Count_"
  ) %>%
  # Convert NA counts to 0
  mutate(across(starts_with("Count_"), ~ replace_na(., 0))) %>%
  arrange(PLAN_NAME_BASE, desc(Count_Less_than_30))

# Append to Excel
addWorksheet(wb, "Broadcaster_Value_Bucket")
writeData(wb, sheet = "Broadcaster_Value_Bucket", city_plan_summary)
saveWorkbook(wb, paste0(broadcaster_name,"_analytical_report.xlsx"), overwrite = TRUE)


# --- [3. Visualizations] ---

# Chart 1: Top Base Plans by Total Customers
baseplan_chart_data <- baseplan_summary %>%
  group_by(PLAN_NAME) %>%
  summarise(Total_Customers = sum(Plan_Customers)) %>%
  arrange(desc(Total_Customers)) %>%
  slice_head(n = 10)

ggplot(baseplan_chart_data, aes(x = reorder(PLAN_NAME, Total_Customers), y = Total_Customers)) +
  geom_col(fill = "#1f77b4") +
  coord_flip() +
  labs(
    title = "Top 10 Most Subscribed Base Plans",
    subtitle = "Total unique subscribers across all cities",
    x = "Base Plan Name",
    y = "Total Unique Subscribers"
  ) +
  theme_minimal(base_size = 12)

ggsave("top_base_plans.png")

# Chart 2: Value Bucket Split by City
city_bucket_data <- customer_value %>%
  group_by(LCO_CITY, Value_Bucket) %>%
  summarise(Customers = n(), .groups = 'drop')

ggplot(city_bucket_data, aes(x = reorder(LCO_CITY, -Customers), y = Customers, fill = Value_Bucket)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    title = "Customer Spending Bucket by City",
    subtitle = "Split of Below ₹55 vs Above ₹55 customers",
    x = "City",
    y = "Customer Count",
    fill = "Spending Category"
  ) +
  theme_minimal(base_size = 12)

ggsave("value_bucket_by_city.png")

writeLines(c(
  "# Analytical Report",
  "",
  "### Charts",
  "",
  "![Top Base Plans](top_base_plans.png)",
  "",
  "![Value Bucket by City](value_bucket_by_city.png)",
  "" ,
  "",
  "## 1. Unique Customer Count per Base Plan",
  kable(baseplan_summary, format = "markdown")
), "analytical_report.md")

# Append to Markdown
write("\n\n## 2. LCO City-wise JioStar Alacarte/Add-on Value Analysis\n", file = "analytical_report.md", append = TRUE)
write(knitr::kable(city_plan_summary, format = "markdown"), file = "analytical_report.md", append = TRUE)


# --- End of Script ---