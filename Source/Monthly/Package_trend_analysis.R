# Package Migration Analysis: Base Package Downgrade with Addon Adoption
# This script analyzes customer behavior after introducing addon packages

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(knitr)

# Read the data files
# Replace with your actual file paths
data_before <- read_csv(file.choose())
data_after <- read_csv(file.choose())

# Clean column names and handle potential formatting issues
clean_data <- function(df) {
  df %>%
    filter(`Plan Type` != "Alacarte") %>%  # Exclude Alacarte plans
    mutate(
      # Remove quotes and clean STB/SC columns if they exist
      Stb = gsub("'", "", Stb),
      Sc = gsub("'", "", Sc),
      # Ensure Customer Nbr is character for joining
      `Customer Nbr` = as.character(`Customer Nbr`)
    )
}

data_before <- clean_data(data_before)
data_after <- clean_data(data_after)

# Define package weights based on your business logic
# You should adjust these weights based on your actual package hierarchy
package_weights <- read_csv(file.choose())

# Function to calculate customer package score (grouped by Customer Number only)
calculate_customer_score <- function(customer_data, weights_df) {
  customer_data %>%
    left_join(weights_df, by = c("Plan Code" = "Plan_Code")) %>%
    group_by(`Customer Nbr`) %>%
    summarise(
      Total_Weight = sum(Weight, na.rm = TRUE),
      Base_Package_Weight = sum(Weight * (!Is_Addon), na.rm = TRUE),
      Addon_Weight = sum(Weight * Is_Addon, na.rm = TRUE),
      Addon_Count = sum(Is_Addon, na.rm = TRUE),
      Total_Services = n(),
      Package_Mix = paste(sort(unique(`Plan Name`)), collapse = ", "),
      Contract_Count = n_distinct(`Contract Number`),  # Track number of contracts
      .groups = "drop"
    )
}

# Calculate scores for both periods
scores_before <- calculate_customer_score(data_before, package_weights)
scores_after <- calculate_customer_score(data_after, package_weights)

# Combine data for comparison
customer_comparison <- scores_before %>%
  rename_with(~paste0(., "_before"), -`Customer Nbr`) %>%
  full_join(
    scores_after %>%
      rename_with(~paste0(., "_after"), -`Customer Nbr`),
    by = "Customer Nbr"
  ) %>%
  # Replace NA with 0 for customers who might have churned or joined
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
  mutate(
    # Calculate changes
    Weight_Change = Total_Weight_after - Total_Weight_before,
    Base_Weight_Change = Base_Package_Weight_after - Base_Package_Weight_before,
    Addon_Change = Addon_Count_after - Addon_Count_before,
    
    # Classify migration patterns
    Migration_Pattern = case_when(
      Weight_Change == 0 ~ "No Change",
      Weight_Change > 0 & Addon_Change > 0 ~ "Upgraded with Addons",
      Weight_Change > 0 & Addon_Change == 0 ~ "Base Package Upgrade",
      Weight_Change < 0 & Addon_Change > 0 ~ "Downgraded Base + Added Addons",
      Weight_Change < 0 & Addon_Change == 0 ~ "Pure Downgrade",
      Weight_Change == 0 & Addon_Change > 0 ~ "Same Base + Added Addons",
      TRUE ~ "Other"
    ),
    
    # Flag customers who changed contracts
    Contract_Change = case_when(
      is.na(Contract_Count_before) ~ "New Customer",
      is.na(Contract_Count_after) ~ "Churned Customer", 
      Contract_Count_before != Contract_Count_after ~ "Contract Changed",
      TRUE ~ "Same Contract"
    )
  )

# Summary Analysis
migration_summary <- customer_comparison %>%
  count(Migration_Pattern, sort = TRUE) %>%
  mutate(Percentage = round(n / sum(n) * 100, 2))

print("Migration Pattern Summary:")
print(migration_summary)

# Key concern analysis: Customers who downgraded base but added addons
concern_customers <- customer_comparison %>%
  filter(Migration_Pattern == "Downgraded Base + Added Addons") %>%
  select(`Customer Nbr`, Weight_Change, Base_Weight_Change, Addon_Change, 
         Package_Mix_before, Package_Mix_after, Contract_Change)

print(paste("Number of customers who downgraded base and added addons:", nrow(concern_customers)))
print("Sample of concerning migrations:")
print(head(concern_customers, 10))

# Additional analysis: Contract changes impact
contract_change_summary <- customer_comparison %>%
  count(Contract_Change, Migration_Pattern) %>%
  arrange(Contract_Change, desc(n))

print("Migration patterns by contract change status:")
print(contract_change_summary)

# ARPU Impact Analysis (Placeholder - you'll need to add pricing data)
# If you provide service pricing, we can calculate actual ARPU impact
calculate_arpu_impact <- function(data_before, data_after, pricing_data = NULL) {
  if (is.null(pricing_data)) {
    cat("To calculate ARPU impact, please provide a pricing_data dataframe with columns:\n")
    cat("- Service_Code (matching your Service Code column)\n")
    cat("- Price (monthly price for each service)\n")
    return(NULL)
  }
  # ARPU calculation code would go here
}

# Visualization
p1 <- ggplot(migration_summary, aes(x = reorder(Migration_Pattern, n), y = n)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  labs(title = "Customer Migration Patterns After Addon Introduction",
       x = "Migration Pattern", y = "Number of Customers") +
  theme_minimal() +
  geom_text(aes(label = paste0(n, " (", Percentage, "%)")), hjust = -0.1)

print(p1)

# Weight distribution before vs after
weight_comparison <- data.frame(
  Period = c(rep("Before", nrow(scores_before)), rep("After", nrow(scores_after))),
  Total_Weight = c(scores_before$Total_Weight, scores_after$Total_Weight),
  Base_Weight = c(scores_before$Base_Package_Weight, scores_after$Base_Package_Weight),
  Addon_Weight = c(scores_before$Addon_Weight, scores_after$Addon_Weight)
)

p2 <- weight_comparison %>%
  pivot_longer(cols = c(Total_Weight, Base_Weight, Addon_Weight), 
               names_to = "Weight_Type", values_to = "Weight") %>%
  ggplot(aes(x = Period, y = Weight, fill = Weight_Type)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~Weight_Type, scales = "free_y") +
  labs(title = "Package Weight Distribution: Before vs After Addon Introduction",
       y = "Package Weight Score") +
  theme_minimal()

print(p2)

# Export results for further analysis
write_csv(customer_comparison, "customer_migration_analysis.csv")
write_csv(migration_summary, "migration_summary.csv")

# Function to add pricing data and recalculate with ARPU
add_pricing_analysis <- function(service_pricing_df) {
  # This function will be used when you provide pricing data
  # Expected format: Service_Code, Price columns
  cat("Pricing analysis function ready. Please provide pricing data in format:\n")
  cat("service_pricing <- data.frame(Service_Code = c(...), Price = c(...))\n")
  cat("Then call: add_pricing_analysis(service_pricing)\n")
}

# Summary insights
cat("\n=== KEY INSIGHTS ===\n")
cat("1. Total customers analyzed:", nrow(customer_comparison), "\n")
cat("2. Customers with concerning migration pattern:", 
    nrow(filter(customer_comparison, Migration_Pattern == "Downgraded Base + Added Addons")), "\n")
cat("3. Most common migration pattern:", 
    migration_summary$Migration_Pattern[1], 
    "(", migration_summary$n[1], "customers,", migration_summary$Percentage[1], "%)\n")

# Risk assessment
risk_percentage <- round(nrow(concern_customers) / nrow(customer_comparison) * 100, 2)
cat("4. Risk Assessment: ", risk_percentage, "% of customers downgraded base and added addons\n")

if (risk_percentage > 10) {
  cat("⚠️  HIGH RISK: Significant customer migration to lower-value packages\n")
} else if (risk_percentage > 5) {
  cat("⚠️  MEDIUM RISK: Notable customer migration pattern\n")
} else {
  cat("✅ LOW RISK: Minimal impact on package downgrades\n")
}
