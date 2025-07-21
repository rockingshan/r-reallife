library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)


#### one file of wallet bill report ####
wallet_bill = read.csv(file.choose())
wallet_bill_p = wallet_bill %>% filter(!(Customer.Nbr == '')) %>% filter(!(Amount.Debit == 0))

# Step 2: Convert Transaction Date to proper date format and add a Month column
wallet_bill_m <- wallet_bill_p %>%
  mutate(
    Bill.Charge.Start.Date = as.Date(Bill.Charge.Start.Date, format = "%d/%m/%Y %I:%M:%S %p"),
    Bill.Charge.End.Date = as.Date(Bill.Charge.End.Date, format = "%d/%m/%Y %I:%M:%S %p"),
    Transaction.Date = as.Date(Transaction.Date, format = "%d/%m/%Y %I:%M:%S %p"),
    Month = format(Transaction.Date, "%Y-%m")
  )
# Step 3: Group by Plan_Details and Month and calculate revenue
monthly_revenue <- wallet_bill_m %>%
  group_by(Plan.Details, Month) %>%
  summarise(
    Total.Revenue = sum(Amount.Debit, na.rm = TRUE),
    Unique.Subs = n_distinct(Customer.Nbr),
    .groups = "drop"
  )

# Step 4: Pivot wider for month-wise comparison
revenue_wide <- monthly_revenue %>%
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  pivot_wider(
    names_from = Month,
    values_from = c(Total.Revenue, Unique.Subs),
    names_glue = "{Month}.{.value}",
    values_fill = 0
  )

##output
write.csv(revenue_wide,"Monthwise_revenue_details.csv",row.names = F)
  



####comparing two months file with wallet new####
# === Step 1: Load Data ===
# Replace with actual file paths
may_file <- file.choose()
june_file <- file.choose()

may <- read_csv(may_file, show_col_types = FALSE)
june <- read_csv(june_file, show_col_types = FALSE)

# === Step 2: Filter for INVOICE document type ===
may <- may %>%
  filter(`Credit Document Type` == "INVOICE") %>%
  mutate(Month = "May")

june <- june %>%
  filter(`Credit Document Type` == "INVOICE") %>%
  mutate(Month = "June")

# === Step 3: Combine and Clean ===
combined <- bind_rows(may, june)

# === Step 4: Entity-Level Summary ===
entity_summary <- combined %>%
  group_by(`Entity Code`, Month) %>%
  summarise(
    Total_Revenue = sum(`Amount Debit`, na.rm = TRUE),
    Unique_Customers = n_distinct(`Customer Nbr`),
    .groups = "drop"
  ) %>%
  mutate(ARPU = ifelse(Unique_Customers > 0, Total_Revenue / Unique_Customers, 0)) %>%
  pivot_wider(
    names_from = Month,
    values_from = c(Total_Revenue, Unique_Customers, ARPU),
    names_sep = "_"
  ) %>%
  mutate(
    Revenue_Diff = Total_Revenue_June - Total_Revenue_May,
    Customer_Diff = Unique_Customers_June - Unique_Customers_May,
    ARPU_Diff = ARPU_June - ARPU_May
  )

# === Step 5: Entity + Plan Summary ===
entity_plan_summary <- combined %>%
  group_by(`Entity Code`, `Plan Details`, Month) %>%
  summarise(
    Revenue = sum(`Amount Debit`, na.rm = TRUE),
    Customer_Count = n_distinct(`Customer Nbr`),
    .groups = "drop"
  ) %>%
  mutate(ARPU = ifelse(Customer_Count > 0, Revenue / Customer_Count, 0)) %>%
  pivot_wider(
    names_from = Month,
    values_from = c(Revenue, Customer_Count, ARPU),
    names_sep = "_",
    values_fill = 0
  ) %>%
  mutate(
    Revenue_Diff = Revenue_June - Revenue_May,
    Customer_Diff = Customer_Count_June - Customer_Count_May,
    ARPU_Diff = ARPU_June - ARPU_May
  )

# === Step 6: Save Outputs ===
write_csv(entity_summary, "Entity_Summary_May_June.csv")
write_csv(entity_plan_summary, "Entity_Plan_Summary_May_June.csv")

cat("✅ Analysis complete. Two CSV files saved:\n- Entity_Summary_May_June.csv\n- Entity_Plan_Summary_May_June.csv\n")
