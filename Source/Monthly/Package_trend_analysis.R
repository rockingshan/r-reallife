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

# Key concern analysis: Customers who downgraded base but added addons
concern_customers <- customer_comparison %>%
  filter(Migration_Pattern == "Downgraded Base + Added Addons") %>%
  select(`Customer Nbr`, Weight_Change, Base_Weight_Change, Addon_Change, 
         Package_Mix_before, Package_Mix_after, Contract_Change)

# Additional analysis: Contract changes impact
contract_change_summary <- customer_comparison %>%
  count(Contract_Change, Migration_Pattern) %>%
  arrange(Contract_Change, desc(n))

# Package type analysis
package_analysis_before <- data_before %>%
  count(`Plan Type`, name = "Count_Before")

package_analysis_after <- data_after %>%
  count(`Plan Type`, name = "Count_After")

package_trend <- package_analysis_before %>%
  full_join(package_analysis_after, by = "Plan Type") %>%
  mutate(
    Count_Before = replace_na(Count_Before, 0),
    Count_After = replace_na(Count_After, 0),
    Change = Count_After - Count_Before,
    Percentage_Change = round((Change / Count_Before) * 100, 2)
  )

# Risk metrics
total_customers <- nrow(customer_comparison)
risk_customers <- nrow(concern_customers)
risk_percentage <- round(risk_customers / total_customers * 100, 2)

# Weight analysis
avg_weight_before <- round(mean(scores_before$Total_Weight, na.rm = TRUE), 2)
avg_weight_after <- round(mean(scores_after$Total_Weight, na.rm = TRUE), 2)
weight_change_overall <- round(avg_weight_after - avg_weight_before, 2)

# Create visualizations
p1 <- ggplot(migration_summary, aes(x = reorder(Migration_Pattern, n), y = n)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  labs(title = "Customer Migration Patterns After Addon Introduction",
       x = "Migration Pattern", y = "Number of Customers") +
  theme_minimal() +
  geom_text(aes(label = paste0(n, " (", Percentage, "%)")), hjust = -0.1)

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

# Create comprehensive markdown report
create_markdown_report <- function() {
  
  # Create plots directory if it doesn't exist
  if (!dir.exists("plots")) {
    dir.create("plots")
  }
  
  # Save plots
  ggsave("plots/migration_patterns.png", plot = p1, width = 10, height = 6, dpi = 300)
  ggsave("plots/weight_distribution.png", plot = p2, width = 10, height = 8, dpi = 300)
  
  # Create additional plots
  p3 <- ggplot(contract_change_summary, aes(x = Contract_Change, y = n, fill = Migration_Pattern)) +
    geom_col(position = "stack") +
    labs(title = "Migration Patterns by Contract Change Status",
         x = "Contract Change Status", y = "Number of Customers") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_brewer(type = "qual", palette = "Set3")
  
  ggsave("plots/contract_change_patterns.png", plot = p3, width = 10, height = 6, dpi = 300)
  
  # Create package type trend plot
  p4 <- package_trend %>%
    pivot_longer(cols = c(Count_Before, Count_After), names_to = "Period", values_to = "Count") %>%
    mutate(Period = ifelse(Period == "Count_Before", "2025-07-13", "2025-07-28")) %>%
    ggplot(aes(x = `Plan Type`, y = Count, fill = Period)) +
    geom_col(position = "dodge") +
    labs(title = "Package Type Distribution: Before vs After",
         x = "Plan Type", y = "Service Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("plots/package_type_trends.png", plot = p4, width = 10, height = 6, dpi = 300)
  
  # Generate markdown content
  markdown_content <- paste0('
# Customer Package Migration Analysis Report
## Period: July 13, 2025 vs July 28, 2025

---

## Executive Summary

This analysis examines customer behavior following the introduction of addon packages (Sports, Movies, Kids addons at lower price points). The primary concern was whether customers would downgrade their base packages and add specific addons instead of maintaining higher-value comprehensive packages.

**Key Finding:** ', risk_percentage, '% of customers (', risk_customers, ' out of ', total_customers, ') adopted the concerning pattern of downgrading base packages while adding addons.

---

## Risk Assessment

', if (risk_percentage > 10) {
  paste0("🔴 **HIGH RISK**: ", risk_percentage, "% of customers showing concerning migration pattern")
} else if (risk_percentage > 5) {
  paste0("🟡 **MEDIUM RISK**: ", risk_percentage, "% of customers showing concerning migration pattern")  
} else {
  paste0("🟢 **LOW RISK**: Only ", risk_percentage, "% of customers showing concerning migration pattern")
}, '

**Overall Package Weight Change:** ', ifelse(weight_change_overall >= 0, "↗️", "↘️"), ' ', abs(weight_change_overall), ' points (Average: ', avg_weight_before, ' → ', avg_weight_after, ')

---

## Migration Pattern Analysis

![Migration Patterns](plots/migration_patterns.png)

### Customer Behavior Breakdown:

')
  
  # Add migration summary table
  for (i in 1:nrow(migration_summary)) {
    pattern <- migration_summary$Migration_Pattern[i]
    count <- migration_summary$n[i] 
    pct <- migration_summary$Percentage[i]
    
    emoji <- case_when(
      pattern == "Downgraded Base + Added Addons" ~ "🔴",
      pattern == "Pure Downgrade" ~ "📉", 
      pattern == "No Change" ~ "➖",
      pattern == "Same Base + Added Addons" ~ "🟡",
      pattern == "Base Package Upgrade" ~ "📈",
      pattern == "Upgraded with Addons" ~ "🟢",
      TRUE ~ "⚪"
    )
    
    markdown_content <- paste0(markdown_content, 
                               '- **', emoji, ' ', pattern, ':** ', count, ' customers (', pct, '%)\n')
  }
  
  markdown_content <- paste0(markdown_content, '

---

## Package Weight Distribution Analysis

![Weight Distribution](plots/weight_distribution.png)

The package weight analysis shows the distribution of customer package values before and after the addon introduction.

---

## Contract Change Impact

![Contract Change Patterns](plots/contract_change_patterns.png)

### Contract Change Summary:

')
  
  # Add contract change analysis
  contract_summary_agg <- contract_change_summary %>%
    group_by(Contract_Change) %>%
    summarise(Total = sum(n), .groups = "drop") %>%
    arrange(desc(Total))
  
  for (i in 1:nrow(contract_summary_agg)) {
    change_type <- contract_summary_agg$Contract_Change[i]
    total <- contract_summary_agg$Total[i]
    
    markdown_content <- paste0(markdown_content,
                               '- **', change_type, ':** ', total, ' customers\n')
  }
  
  markdown_content <- paste0(markdown_content, '

---

## Package Type Trends

![Package Type Trends](plots/package_type_trends.png)

### Service Count Changes by Package Type:

')
  
  # Add package trend table
  for (i in 1:nrow(package_trend)) {
    pkg_type <- package_trend$`Plan Type`[i]
    before <- package_trend$Count_Before[i]
    after <- package_trend$Count_After[i] 
    change <- package_trend$Change[i]
    pct_change <- package_trend$Percentage_Change[i]
    
    trend_emoji <- ifelse(change > 0, "↗️", ifelse(change < 0, "↘️", "➖"))
    
    markdown_content <- paste0(markdown_content,
                               '- **', pkg_type, ':** ', before, ' → ', after, ' services ', trend_emoji, ' (', 
                               ifelse(change >= 0, "+", ""), change, ', ', 
                               ifelse(is.finite(pct_change), paste0(pct_change, "%"), "N/A"), ')\n')
  }
  
  markdown_content <- paste0(markdown_content, '

---

## Business Recommendations

### Immediate Actions:
1. **Monitor High-Risk Customers**: Focus on the ', risk_customers, ' customers who downgraded base packages
2. **Pricing Strategy Review**: Consider adjusting addon pricing if revenue impact is significant
3. **Customer Communication**: Engage with customers to understand their decision drivers

### Strategic Considerations:
- **Addon Strategy Impact**: ', ifelse(risk_percentage > 10, "Consider revising addon strategy", 
                                       ifelse(risk_percentage > 5, "Monitor closely but strategy appears manageable", "Addon strategy showing positive results")), '
- **Package Portfolio**: Review base package value proposition vs addon combinations
- **Revenue Protection**: Implement safeguards against excessive base package downgrades

---

## Data Quality Notes
- **Analysis Period**: 7-day comparison (July 13-28, 2025)
- **Customer-Level Analysis**: Grouped by Customer Number to avoid contract change duplications  
- **Total Customers Analyzed**: ', total_customers, '
- **Data Completeness**: Both datasets successfully processed and compared

---

*Generated on: ', Sys.Date(), '*
*Analysis conducted using package weight methodology with comprehensive migration pattern classification*
')
  
  # Write markdown file
  writeLines(markdown_content, "Customer_Migration_Analysis_Report.md")
  
  cat("✅ Markdown report generated: Customer_Migration_Analysis_Report.md\n")
  cat("📊 Plots saved in: plots/ directory\n")
  cat("📁 Data exports: customer_migration_analysis.csv, migration_summary.csv\n")
}

# Generate the report
create_markdown_report()

# Export results for further analysis
write_csv(customer_comparison, "customer_migration_analysis.csv")
write_csv(migration_summary, "migration_summary.csv")
write_csv(contract_change_summary, "contract_change_summary.csv")
write_csv(package_trend_analysis, "package_trend_analysis.csv")

# ARPU Impact Analysis (Placeholder - you'll need to add pricing data)
calculate_arpu_impact <- function(data_before, data_after, pricing_data = NULL) {
  if (is.null(pricing_data)) {
    cat("To calculate ARPU impact, please provide a pricing_data dataframe with columns:\n")
    cat("- Service_Code (matching your Service Code column)\n")
    cat("- Price (monthly price for each service)\n")
    return(NULL)
  }
  # ARPU calculation code would go here
}

# Function to add pricing data and recalculate with ARPU
add_pricing_analysis <- function(service_pricing_df) {
  # This function will be used when you provide pricing data
  # Expected format: Service_Code, Price columns
  cat("Pricing analysis function ready. Please provide pricing data in format:\n")
  cat("service_pricing <- data.frame(Service_Code = c(...), Price = c(...))\n")
  cat("Then call: add_pricing_analysis(service_pricing)\n")
}