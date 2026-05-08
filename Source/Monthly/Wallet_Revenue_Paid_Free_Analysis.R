library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(zoo)

cat("========================================\n")
cat("WALLET REVENUE & CUSTOMER ANALYSIS\n")
cat("========================================\n\n")

# Step 1: Load wallet transaction data
cat("Step 1: Loading wallet transaction data...\n")
wallet_file <- file.choose()
wallet_data <- read.csv(wallet_file)
cat("   Loaded", nrow(wallet_data), "rows\n\n")


# Step 2: Load LCO Master Report for City mapping
cat("Step 2: Loading LCO Master Report for city mapping...\n")
lco_master_file <- file.choose()
lco_master <- read.csv(lco_master_file, header = TRUE) %>%
  dplyr::rename(Entity.Code = `Lco.Code`) %>%   # convert new header to expected name
  dplyr::select(Entity.Code, City) %>%
  dplyr::distinct()
lco_master$Entity.Code <- gsub("'","",lco_master$Entity.Code) # Remove any leading apostrophes from Entity Codes
cat("   Loaded", nrow(lco_master), "LCO-City mappings\n\n")


# Step 3: Load valid base plan names
cat("Step 3: Loading valid base plan names...\n")
valid_plans_file <- file.choose()
valid_plans <- read.csv(valid_plans_file, header = TRUE)
valid_plan_names <- valid_plans$Bouquet
cat("   Loaded", length(valid_plan_names), "valid base plans\n\n")


# Step 4: Clean and prepare wallet data
cat("Step 4: Preparing wallet data...\n")
wallet_clean <- wallet_data %>%
  filter(!is.na(Customer.Nbr) & Customer.Nbr != "") %>%
  mutate(
    # Serialize Transaction Date to extract month
    Transaction.Date = as.Date(Transaction.Date, format = "%d/%m/%Y %H:%M"),
    Month = format(Transaction.Date, "%Y-%m"),
    # Convert Amount Debit to numeric (remove any non-numeric characters)
    Amount.Debit = as.numeric(gsub("[^0-9.]", "", as.character(Amount.Debit))),
    # Replace NA with 0
    Amount.Debit = ifelse(is.na(Amount.Debit), 0, Amount.Debit)
  ) 

cat("   Cleaned data has", nrow(wallet_clean), "rows\n")
cat("   Date range:", min(wallet_clean$Transaction.Date, na.rm = TRUE), "to",
    max(wallet_clean$Transaction.Date, na.rm = TRUE), "\n\n")

# Step 4b: Replace demo entity codes with actual entity codes derived from Customer.Nbr
cat("Step 4b: Resolving demo entity codes to actual entities...\n")

demo_entities <- c("MDBQA","MDCNDP","MDHCNJV","MDSKWJV","MDKH")

wallet_clean <- wallet_clean %>%
  mutate(
    Derived.Entity.Code = sub("^[0-9]+", "", Customer.Nbr),
    Entity.Code = ifelse(Entity.Code %in% demo_entities,
                         Derived.Entity.Code,
                         Entity.Code)
  ) %>%
  select(-Derived.Entity.Code) %>%
  # Join with LCO Master to add City information
  left_join(lco_master, by = "Entity.Code")

cat("   Demo entity corrections applied\n\n")

# Step 5: Filter for valid base plans only
cat("Step 5: Filtering for valid base plans...\n")
wallet_valid_plans <- wallet_clean %>%
  filter(Plan.Details %in% valid_plan_names)

cat("   Filtered to", nrow(wallet_valid_plans), "rows with valid base plans\n")
cat("   Unique customers:", n_distinct(wallet_valid_plans$Customer.Nbr), "\n")
cat("   Cities covered:", n_distinct(wallet_valid_plans$City, na.rm = TRUE), "\n\n")

# Step 6: Calculate monthly metrics by LCO
cat("Step 6: Calculating monthly metrics by LCO...\n\n")

# Group by Entity Code (LCO), City, and Month
monthly_lco_summary <- wallet_valid_plans %>%
  group_by(Entity.Code, City, Month) %>%
  summarise(
    # Total unique customers (all with valid base plans)
    Total_Customers = n_distinct(Customer.Nbr),

    # Paid customers (Amount Debit != 0)
    Paid_Customers = n_distinct(Customer.Nbr[Amount.Debit != 0]),

    # Free customers (Amount Debit == 0)
    Free_Customers = n_distinct(Customer.Nbr[Amount.Debit == 0]),

    # Total revenue (sum of Amount Debit)
    Total_Revenue = sum(Amount.Debit, na.rm = TRUE),

    .groups = "drop"
  ) %>%
  mutate(
    # Calculate ARPU (Average Revenue Per User) based on all subscribers
    ARPU = ifelse(Total_Customers > 0, Total_Revenue / Total_Customers, 0),

    # Round values for readability
    Total_Revenue = round(Total_Revenue, 2),
    ARPU = round(ARPU, 2)
  )

# Step 7: Create wide format for easy month-by-month comparison
cat("Step 7: Creating month-wise comparison table...\n\n")

# Pivot to wide format with each month as columns
monthly_wide <- monthly_lco_summary %>%
  pivot_wider(
    id_cols = c(Entity.Code, City),
    names_from = Month,
    values_from = c(Total_Customers, Paid_Customers, Free_Customers, Total_Revenue, ARPU),
    names_glue = "{Month}_{.value}",
    values_fill = 0
  ) %>%
  arrange(Entity.Code)

# Step 8: Create a detailed monthly summary (long format)
monthly_long <- monthly_lco_summary %>%
  arrange(Entity.Code, Month)

# Step 9: Create overall summary by month (all LCOs combined)
cat("Step 8: Creating overall monthly summary...\n\n")

overall_monthly_summary <- wallet_valid_plans %>%
  group_by(Month) %>%
  summarise(
    Total_Customers = n_distinct(Customer.Nbr),
    Paid_Customers = n_distinct(Customer.Nbr[Amount.Debit != 0]),
    Free_Customers = n_distinct(Customer.Nbr[Amount.Debit == 0]),
    Total_Revenue = sum(Amount.Debit, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ARPU = ifelse(Total_Customers > 0, Total_Revenue / Total_Customers, 0),
    Total_Revenue = round(Total_Revenue, 2),
    ARPU = round(ARPU, 2)
  ) %>%
  arrange(Month)

# Step 10: Customer Journey Analysis (Free to Paid Conversion)
cat("Step 9: Analyzing customer journey (Free to Paid conversion)...\n")

# Create customer-level summary to track payment behavior over entire duration
customer_journey <- wallet_valid_plans %>%
  group_by(Entity.Code, City, Customer.Nbr) %>%
  summarise(
    # Check if customer ever had free service (Amount Debit = 0)
    Had_Free = any(Amount.Debit == 0),
    # Check if customer ever paid (Amount Debit != 0)
    Had_Paid = any(Amount.Debit != 0),
    # First transaction date
    First_Transaction = min(Transaction.Date, na.rm = TRUE),
    # Last transaction date
    Last_Transaction = max(Transaction.Date, na.rm = TRUE),
    # Total amount paid over entire period
    Total_Paid = sum(Amount.Debit, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Classify customer journey
    Customer_Journey = case_when(
      Had_Free & Had_Paid ~ "Free_to_Paid",  # Started free, became paid
      Had_Free & !Had_Paid ~ "Always_Free",  # Always free, never paid
      !Had_Free & Had_Paid ~ "Always_Paid",  # Always paid, never free
      TRUE ~ "Unknown"
    )
  )

# Aggregate by LCO for the entire duration
lco_journey_summary <- customer_journey %>%
  group_by(Entity.Code, City) %>%
  summarise(
    # Count customers in each category
    Always_Paid_Count = sum(Customer_Journey == "Always_Paid"),
    Free_to_Paid_Count = sum(Customer_Journey == "Free_to_Paid"),
    Always_Free_Count = sum(Customer_Journey == "Always_Free"),

    # Total customers
    Total_Customers = n(),

    # Calculate percentages
    Always_Paid_Pct = round((Always_Paid_Count / Total_Customers) * 100, 2),
    Free_to_Paid_Pct = round((Free_to_Paid_Count / Total_Customers) * 100, 2),
    Always_Free_Pct = round((Always_Free_Count / Total_Customers) * 100, 2),

    # Revenue metrics
    Total_Revenue_All = sum(Total_Paid, na.rm = TRUE),
    Revenue_from_Converted = sum(Total_Paid[Customer_Journey == "Free_to_Paid"], na.rm = TRUE),
    Revenue_from_Always_Paid = sum(Total_Paid[Customer_Journey == "Always_Paid"], na.rm = TRUE),

    .groups = "drop"
  ) %>%
  mutate(
    # Calculate conversion rate (Free to Paid / (Free to Paid + Always Free))
    Conversion_Rate_Pct = ifelse((Free_to_Paid_Count + Always_Free_Count) > 0,
                                  round((Free_to_Paid_Count / (Free_to_Paid_Count + Always_Free_Count)) * 100, 2),
                                  0),

    # Round revenue values
    Total_Revenue_All = round(Total_Revenue_All, 2),
    Revenue_from_Converted = round(Revenue_from_Converted, 2),
    Revenue_from_Always_Paid = round(Revenue_from_Always_Paid, 2)
  ) %>%
  arrange(Entity.Code)

# Overall summary across all LCOs
overall_journey_summary <- customer_journey %>%
  summarise(
    Always_Paid_Count = sum(Customer_Journey == "Always_Paid"),
    Free_to_Paid_Count = sum(Customer_Journey == "Free_to_Paid"),
    Always_Free_Count = sum(Customer_Journey == "Always_Free"),
    Total_Customers = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Always_Paid_Pct = round((Always_Paid_Count / Total_Customers) * 100, 2),
    Free_to_Paid_Pct = round((Free_to_Paid_Count / Total_Customers) * 100, 2),
    Always_Free_Pct = round((Always_Free_Count / Total_Customers) * 100, 2),
    Conversion_Rate_Pct = ifelse((Free_to_Paid_Count + Always_Free_Count) > 0,
                                  round((Free_to_Paid_Count / (Free_to_Paid_Count + Always_Free_Count)) * 100, 2),
                                  0),

    Total_Revenue = sum(customer_journey$Total_Paid, na.rm = TRUE),
    Revenue_from_Converted = sum(customer_journey$Total_Paid[customer_journey$Customer_Journey == "Free_to_Paid"], na.rm = TRUE),
    Revenue_from_Always_Paid = sum(customer_journey$Total_Paid[customer_journey$Customer_Journey == "Always_Paid"], na.rm = TRUE)
  ) %>%
  mutate(
    Total_Revenue = round(Total_Revenue, 2),
    Revenue_from_Converted = round(Revenue_from_Converted, 2),
    Revenue_from_Always_Paid = round(Revenue_from_Always_Paid, 2)
  )

cat("   Completed customer journey analysis\n\n")

# Step 11: Customer-Level Monthly Bill Amount (Wide Format)
cat("Step 11: Creating customer-level monthly bill amount table...\n")

customer_monthly_bill <- wallet_valid_plans %>%
  group_by(Customer.Nbr, Entity.Code, Month) %>%
  summarise(
    Bill_Amount = sum(Amount.Debit, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(Customer.Nbr, Entity.Code),
    names_from = Month,
    values_from = Bill_Amount
  ) %>%
  arrange(Customer.Nbr)

output_file_10 <- "Wallet_Customer_Monthly_Bill_Amount.csv"
write.csv(customer_monthly_bill, output_file_10, row.names = FALSE)
cat("   Saved:", output_file_10, "\n\n")

###Additional analysis added on 2026-03-10
# Step X1: Free → Paid Conversion Lag Analysis
cat("Calculating Free → Paid conversion lag...\n")

customer_conversion_lag <- wallet_valid_plans %>%
  group_by(Customer.Nbr) %>%
  summarise(
    First_Free_Date = if (any(Amount.Debit == 0))
        min(Transaction.Date[Amount.Debit == 0])
      else as.Date(NA),

    First_Paid_Date = if (any(Amount.Debit != 0))
        min(Transaction.Date[Amount.Debit != 0])
      else as.Date(NA),

    .groups = "drop"
  ) %>%
  mutate(
    Lag_Months =
      ifelse(
        is.na(First_Free_Date) | is.na(First_Paid_Date),
        NA,
        (as.numeric(format(First_Paid_Date,"%Y")) -
         as.numeric(format(First_Free_Date,"%Y"))) * 12 +
        (as.numeric(format(First_Paid_Date,"%m")) -
         as.numeric(format(First_Free_Date,"%m")))
      ),

    Lag_Category = case_when(
      is.na(Lag_Months) ~ "Never_Converted",
      Lag_Months <= 1 ~ "Convert_1_Month",
      Lag_Months <= 2 ~ "Convert_2_Month",
      TRUE ~ "Convert_3plus_Month"
    )
  )

conversion_lag_summary <- customer_conversion_lag %>%
  count(Lag_Category)

write.csv(customer_conversion_lag,
          "Customer_Conversion_Lag_Details.csv",
          row.names = FALSE)

write.csv(conversion_lag_summary,
          "Customer_Conversion_Lag_Summary.csv",
          row.names = FALSE)


#Free Abuse Pattern Detection
cat("Calculating Free abuse patterns...\n")

free_abuse_customers <- wallet_valid_plans %>%
  filter(Amount.Debit == 0) %>%
  group_by(Customer.Nbr, Entity.Code) %>%
  summarise(
    Free_Months = n_distinct(Month),
    .groups = "drop"
  )

free_abuse_summary <- free_abuse_customers %>%
  group_by(Entity.Code) %>%
  summarise(
    Customers_with_Free = n(),
    Avg_Free_Months = round(mean(Free_Months),2),
    Max_Free_Months = max(Free_Months),
    Multi_Free_Customers = sum(Free_Months > 1),
    .groups = "drop"
  ) %>%
  arrange(desc(Avg_Free_Months))

write.csv(free_abuse_customers,
          "Customer_Free_Months_Details.csv",
          row.names = FALSE)

write.csv(free_abuse_summary,
          "LCO_Free_Abuse_Summary.csv",
          row.names = FALSE)

#Activation Month Cohort Analysis
cat("Calculating Activation Cohort Analysis...\n")

customer_activation <- wallet_valid_plans %>%
  group_by(Customer.Nbr) %>%
  summarise(
    Activation_Month = min(Month),
    .groups = "drop"
  )

cohort_data <- wallet_valid_plans %>%
  left_join(customer_activation, by = "Customer.Nbr") %>%
  mutate(
    Month_Index = as.numeric(as.yearmon(Month) -
                             as.yearmon(Activation_Month))
  )

cohort_summary <- cohort_data %>%
  group_by(Activation_Month, Month_Index) %>%
  summarise(
    Customers = n_distinct(Customer.Nbr),
    Paid_Customers = n_distinct(Customer.Nbr[Amount.Debit != 0]),
    .groups = "drop"
  )

write.csv(cohort_summary,
          "Activation_Cohort_Analysis.csv",
          row.names = FALSE)

#Revenue Loss from Customers Who Never Paid
cat("Calculating revenue loss from never-paid customers...\n")

avg_arpu <- mean(wallet_valid_plans$Amount.Debit[wallet_valid_plans$Amount.Debit > 0], na.rm = TRUE)

never_paid <- customer_journey %>%
  filter(Customer_Journey == "Always_Free")

revenue_loss_summary <- never_paid %>%
  group_by(Entity.Code) %>%
  summarise(
    Always_Free_Customers = n(),
    Estimated_Revenue_Loss = round(Always_Free_Customers * avg_arpu,2),
    .groups = "drop"
  ) %>%
  arrange(desc(Estimated_Revenue_Loss))

write.csv(revenue_loss_summary,
          "LCO_Revenue_Loss_from_Free_Customers.csv",
          row.names = FALSE)

#Retention Curve (Free User Retention)
cat("Generating retention curve data...\n")

free_users <- wallet_valid_plans %>%
  filter(Amount.Debit == 0)

first_free <- free_users %>%
  group_by(Customer.Nbr) %>%
  summarise(
    First_Free_Month = min(Month),
    .groups = "drop"
  )

retention_data <- wallet_valid_plans %>%
  left_join(first_free, by = "Customer.Nbr") %>%
  filter(!is.na(First_Free_Month)) %>%
  mutate(
    Month_Index = as.numeric(as.yearmon(Month) -
                             as.yearmon(First_Free_Month))
  )

retention_curve <- retention_data %>%
  group_by(Month_Index) %>%
  summarise(
    Customers = n_distinct(Customer.Nbr),
    Paid_Customers = n_distinct(Customer.Nbr[Amount.Debit != 0]),
    Free_Customers = n_distinct(Customer.Nbr[Amount.Debit == 0]),
    .groups = "drop"
  ) %>%
  arrange(Month_Index)

write.csv(retention_curve,
          "Free_User_Retention_Curve.csv",
          row.names = FALSE)


# Step 11: City-Level Analysis
cat("Step 10: Performing city-level analysis...\n")

# 11a: City-level monthly summary
city_monthly_summary <- wallet_valid_plans %>%
  group_by(City, Month) %>%
  summarise(
    Total_Customers = n_distinct(Customer.Nbr),
    Paid_Customers = n_distinct(Customer.Nbr[Amount.Debit != 0]),
    Free_Customers = n_distinct(Customer.Nbr[Amount.Debit == 0]),
    Total_Revenue = sum(Amount.Debit, na.rm = TRUE),
    LCO_Count = n_distinct(Entity.Code),
    .groups = "drop"
  ) %>%
  mutate(
    ARPU = ifelse(Total_Customers > 0, Total_Revenue / Total_Customers, 0),
    Total_Revenue = round(Total_Revenue, 2),
    ARPU = round(ARPU, 2)
  ) %>%
  arrange(City, Month)

# 11b: City-level revenue degradation analysis
# Get first and last month for each city
city_revenue_trend <- city_monthly_summary %>%
  group_by(City) %>%
  summarise(
    First_Month = min(Month),
    Last_Month = max(Month),
    First_Month_Revenue = Total_Revenue[Month == min(Month)][1],
    Last_Month_Revenue = Total_Revenue[Month == max(Month)][1],
    First_Month_Customers = Total_Customers[Month == min(Month)][1],
    Last_Month_Customers = Total_Customers[Month == max(Month)][1],
    First_Month_ARPU = ARPU[Month == min(Month)][1],
    Last_Month_ARPU = ARPU[Month == max(Month)][1],
    .groups = "drop"
  ) %>%
  mutate(
    Revenue_Change = Last_Month_Revenue - First_Month_Revenue,
    Revenue_Change_Pct = ifelse(First_Month_Revenue > 0,
                                round((Revenue_Change / First_Month_Revenue) * 100, 2),
                                0),
    Customer_Change = Last_Month_Customers - First_Month_Customers,
    Customer_Change_Pct = ifelse(First_Month_Customers > 0,
                                  round((Customer_Change / First_Month_Customers) * 100, 2),
                                  0),
    ARPU_Change = Last_Month_ARPU - First_Month_ARPU,
    ARPU_Change_Pct = ifelse(First_Month_ARPU > 0,
                              round((ARPU_Change / First_Month_ARPU) * 100, 2),
                              0),
    Trend_Status = case_when(
      Revenue_Change_Pct < -5 ~ "Declining",
      Revenue_Change_Pct > 5 ~ "Growing",
      TRUE ~ "Stable"
    )
  ) %>%
  arrange(desc(Revenue_Change_Pct))

# 11c: City-level customer journey summary
city_journey_summary <- customer_journey %>%
  group_by(City) %>%
  summarise(
    Always_Paid_Count = sum(Customer_Journey == "Always_Paid"),
    Free_to_Paid_Count = sum(Customer_Journey == "Free_to_Paid"),
    Always_Free_Count = sum(Customer_Journey == "Always_Free"),
    Total_Customers = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Always_Paid_Pct = round((Always_Paid_Count / Total_Customers) * 100, 2),
    Free_to_Paid_Pct = round((Free_to_Paid_Count / Total_Customers) * 100, 2),
    Always_Free_Pct = round((Always_Free_Count / Total_Customers) * 100, 2),
    Conversion_Rate_Pct = ifelse((Free_to_Paid_Count + Always_Free_Count) > 0,
                                  round((Free_to_Paid_Count / (Free_to_Paid_Count + Always_Free_Count)) * 100, 2),
                                  0),
    Total_Revenue = sum(customer_journey$Total_Paid[customer_journey$City == City], na.rm = TRUE),
    Revenue_from_Converted = sum(customer_journey$Total_Paid[customer_journey$City == City &
                                                               customer_journey$Customer_Journey == "Free_to_Paid"], na.rm = TRUE),
    Revenue_from_Always_Paid = sum(customer_journey$Total_Paid[customer_journey$City == City &
                                                                 customer_journey$Customer_Journey == "Always_Paid"], na.rm = TRUE)
  ) %>%
  mutate(
    Total_Revenue = round(Total_Revenue, 2),
    Revenue_from_Converted = round(Revenue_from_Converted, 2),
    Revenue_from_Always_Paid = round(Revenue_from_Always_Paid, 2)
  ) %>%
  arrange(City)

# 11d: City-level wide format (month-wise)
city_monthly_wide <- city_monthly_summary %>%
  pivot_wider(
    id_cols = City,
    names_from = Month,
    values_from = c(Total_Customers, Paid_Customers, Free_Customers, Total_Revenue, ARPU),
    names_glue = "{Month}_{.value}",
    values_fill = 0
  ) %>%
  arrange(City)

cat("   Completed city-level analysis\n")
cat("   Cities analyzed:", n_distinct(city_monthly_summary$City, na.rm = TRUE), "\n\n")

# Step 12: Save outputs
cat("Step 11: Saving analysis outputs...\n")

# LCO-Level Files
output_file_1 <- "Wallet_LCO_Monthly_Wide.csv"
output_file_2 <- "Wallet_LCO_Monthly_Details.csv"
output_file_3 <- "Wallet_Overall_Monthly_Summary.csv"
output_file_4 <- "Wallet_LCO_Customer_Journey_Summary.csv"
output_file_5 <- "Wallet_Overall_Customer_Journey_Summary.csv"

# City-Level Files
output_file_6 <- "Wallet_City_Monthly_Wide.csv"
output_file_7 <- "Wallet_City_Monthly_Details.csv"
output_file_8 <- "Wallet_City_Revenue_Trend_Analysis.csv"
output_file_9 <- "Wallet_City_Customer_Journey_Summary.csv"

# Save LCO-level files
write.csv(monthly_wide, output_file_1, row.names = FALSE)
cat("   Saved:", output_file_1, "\n")

write.csv(monthly_long, output_file_2, row.names = FALSE)
cat("   Saved:", output_file_2, "\n")

write.csv(overall_monthly_summary, output_file_3, row.names = FALSE)
cat("   Saved:", output_file_3, "\n")

write.csv(lco_journey_summary, output_file_4, row.names = FALSE)
cat("   Saved:", output_file_4, "\n")

write.csv(overall_journey_summary, output_file_5, row.names = FALSE)
cat("   Saved:", output_file_5, "\n")

# Save City-level files
write.csv(city_monthly_wide, output_file_6, row.names = FALSE)
cat("   Saved:", output_file_6, "\n")

write.csv(city_monthly_summary, output_file_7, row.names = FALSE)
cat("   Saved:", output_file_7, "\n")

write.csv(city_revenue_trend, output_file_8, row.names = FALSE)
cat("   Saved:", output_file_8, "\n")

write.csv(city_journey_summary, output_file_9, row.names = FALSE)
cat("   Saved:", output_file_9, "\n\n")

# Display summary statistics
cat("========================================\n")
cat("ANALYSIS SUMMARY\n")
cat("========================================\n\n")

cat("Total LCOs analyzed:", n_distinct(monthly_lco_summary$Entity.Code), "\n")
cat("Total Cities covered:", n_distinct(city_monthly_summary$City, na.rm = TRUE), "\n")
cat("Months covered:", paste(unique(monthly_lco_summary$Month), collapse = ", "), "\n\n")

cat("Overall Monthly Summary:\n")
print(overall_monthly_summary)

cat("\n----------------------------------------\n")
cat("Customer Journey Summary:\n")
print(overall_journey_summary)

cat("\n----------------------------------------\n")
cat("City Revenue Trend Summary:\n")
print(city_revenue_trend %>% select(City, Trend_Status, Revenue_Change_Pct, ARPU_Change_Pct))

cat("\n========================================\n")
cat("Analysis complete! 10 files generated.\n")
cat("  - 5 LCO-level files\n")
cat("  - 4 City-level files\n")
cat("  - 1 Customer-level file\n")
cat("========================================\n")
