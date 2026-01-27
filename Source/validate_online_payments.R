# ============================================================================
# Online Payment Validation Script
# Purpose: Validate and map CCAvenue online transactions with CRM payment data
# ============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(writexl)
library(lubridate)

cat("========================================\n")
cat("ONLINE PAYMENT VALIDATION TOOL\n")
cat("========================================\n\n")

# ============================================================================
# 1. FILE SELECTION
# ============================================================================

cat("STEP 1: Select data files\n")
cat("---------------------------\n\n")

# Select CCAvenue transaction data file
# USER ACTION: Browse and select the CCAvenue online transaction vendor CSV file
#              File name example: apr-dec.csv
#              This file contains: CCAvenue Ref#, Order No, Merchant Param1, Order Amount, etc.
cat("Please select the CCAvenue ONLINE TRANSACTION VENDOR file\n")
cat("(Example: apr-dec.csv)\n")
cat("This file should contain: CCAvenue Ref#, Order No, Merchant Param1, Order Amount\n\n")
Sys.sleep(1)  # Brief pause to let user read the message

ccavenue_file <- file.choose()
cat("Selected:", ccavenue_file, "\n\n")

# Select CRM payment report file
# USER ACTION: Browse and select the CRM ListofPaymentsReport CSV file
#              File name example: 16823409_ListofPaymentsReport_APR-DEC25.CSV
#              This file contains: Receipt#, Payment Nbr#, Entity Code, Amount, Pay Mode, etc.
cat("Please select the CRM PAYMENT REPORT file\n")
cat("(Example: 16823409_ListofPaymentsReport_APR-DEC25.CSV)\n")
cat("This file should contain: Receipt#, Payment Nbr#, Entity Code, Amount, Pay Mode\n\n")
Sys.sleep(1)

crm_file <- file.choose()
cat("Selected:", crm_file, "\n\n")

# Select output directory
# USER ACTION: Browse and select the folder where you want to save the output files
cat("Please select the OUTPUT DIRECTORY where results will be saved\n\n")
Sys.sleep(1)

output_dir <- choose.dir(default = dirname(ccavenue_file),
                         caption = "Select Output Directory for Results")

if(is.na(output_dir)) {
  cat("No output directory selected. Using the same directory as CCAvenue file.\n")
  output_dir <- dirname(ccavenue_file)
}

cat("Output will be saved to:", output_dir, "\n\n")

# ============================================================================
# 2. LOAD DATA
# ============================================================================

cat("========================================\n")
cat("STEP 2: Loading data files\n")
cat("========================================\n\n")

# Load CCAvenue transaction data
cat("Loading CCAvenue transaction data...\n")
ccavenue_data <- read_csv(ccavenue_file,
                          col_types = cols(.default = "c"),
                          locale = locale(encoding = "UTF-8"))

cat("✓ CCAvenue records loaded:", nrow(ccavenue_data), "\n\n")

# Load CRM payment data
cat("Loading CRM payment data...\n")
crm_data <- read_csv(crm_file,
                     col_types = cols(.default = "c"),
                     locale = locale(encoding = "UTF-8"))

cat("✓ CRM records loaded:", nrow(crm_data), "\n\n")

# ============================================================================
# 3. DATA CLEANING AND PREPARATION
# ============================================================================

cat("========================================\n")
cat("STEP 3: Cleaning and preparing data\n")
cat("========================================\n\n")

# Clean CCAvenue data
ccavenue_clean <- ccavenue_data %>%
  mutate(
    CCAvenue_Ref = `CCAvenue Ref#`,
    Order_No = `Order No`,
    Order_Datetime = `Order Datetime`,
    Payment_Mode_Vendor = `Payment Mode`,
    Card_Type = `Card Type`,
    Entity_Code_Vendor = `Merchant Param1`,
    Entity_Name_Vendor = `Merchant Param2`,
    Order_Amount = as.numeric(`Order Amount`),
    Order_Status = `Order Status`,
    Bank_Ref_No = order_bank_ref_no,
    Bill_Email = `Bill Email`,
    Bill_Tel = `Bill Tel`,
    VPA = order_vpa
  ) %>%
  select(CCAvenue_Ref, Order_No, Order_Datetime, Payment_Mode_Vendor,
         Card_Type, Entity_Code_Vendor, Entity_Name_Vendor, Order_Amount,
         Order_Status, Bank_Ref_No, Bill_Email, Bill_Tel, VPA)

# Clean CRM data and filter for online payments
crm_clean <- crm_data %>%
  filter(`Pay Mode` %in% c("ONLINE PAYMENT", "Online Transfer")) %>%
  mutate(
    Payment_Nbr = `Payment Nbr#`,
    Receipt_No = `Receipt#`,
    Reference_No = `Reference Number`,
    Entity_Code_CRM = `Entity Code`,
    Entity_Name_CRM = `Entity Name`,
    Payment_Date = `Payment Date`,
    Pay_Mode = `Pay Mode`,
    Amount_CRM = as.numeric(Amount),
    Customer_Name = `Customer Name`,
    City = City,
    Transaction_Number = `Transaction Number`,
    Notes = NOTES_18
  ) %>%
  select(Payment_Nbr, Receipt_No, Reference_No, Entity_Code_CRM,
         Entity_Name_CRM, Payment_Date, Pay_Mode, Amount_CRM,
         Customer_Name, City, Transaction_Number, Notes)

cat("✓ Filtered CRM online payments:", nrow(crm_clean), "\n")
cat("  Unique Pay Modes in filtered data:",
    paste(unique(crm_clean$Pay_Mode), collapse = ", "), "\n\n")

# ============================================================================
# 4. DATA MATCHING
# ============================================================================

cat("========================================\n")
cat("STEP 4: Matching records between systems\n")
cat("========================================\n\n")

# Match CRM to CCAvenue (left join from CRM perspective)
crm_matched <- crm_clean %>%
  left_join(ccavenue_clean,
            by = c("Receipt_No" = "CCAvenue_Ref")) %>%
  mutate(
    Match_Status = case_when(
      is.na(Order_No) ~ "NOT_IN_CCAVENUE",
      TRUE ~ "MATCHED"
    ),
    Amount_Difference = abs(Amount_CRM - Order_Amount),
    Amount_Match = case_when(
      is.na(Order_Amount) ~ "NO_VENDOR_DATA",
      abs(Amount_CRM - Order_Amount) < 0.01 ~ "EXACT_MATCH",
      abs(Amount_CRM - Order_Amount) <= 1 ~ "MINOR_DIFF",
      TRUE ~ "MISMATCH"
    ),
    Entity_Code_Match = case_when(
      is.na(Entity_Code_Vendor) ~ "NO_VENDOR_DATA",
      Entity_Code_CRM == Entity_Code_Vendor ~ "MATCH",
      TRUE ~ "MISMATCH"
    ),
    Reference_Match = case_when(
      is.na(Order_No) ~ "NO_VENDOR_DATA",
      is.na(Reference_No) | Reference_No == "" ~ "NO_CRM_REF",
      Reference_No == Order_No ~ "MATCH",
      TRUE ~ "MISMATCH"
    )
  )

# Match CCAvenue to CRM (left join from CCAvenue perspective)
ccavenue_matched <- ccavenue_clean %>%
  left_join(crm_clean,
            by = c("CCAvenue_Ref" = "Receipt_No")) %>%
  mutate(
    Match_Status = case_when(
      is.na(Payment_Nbr) ~ "NOT_IN_CRM",
      TRUE ~ "MATCHED"
    ),
    Amount_Difference = abs(Order_Amount - Amount_CRM),
    Amount_Match = case_when(
      is.na(Amount_CRM) ~ "NO_CRM_DATA",
      abs(Order_Amount - Amount_CRM) < 0.01 ~ "EXACT_MATCH",
      abs(Order_Amount - Amount_CRM) <= 1 ~ "MINOR_DIFF",
      TRUE ~ "MISMATCH"
    ),
    Entity_Code_Match = case_when(
      is.na(Entity_Code_CRM) ~ "NO_CRM_DATA",
      Entity_Code_Vendor == Entity_Code_CRM ~ "MATCH",
      TRUE ~ "MISMATCH"
    )
  )

# ============================================================================
# 5. IDENTIFY ANOMALIES AND DUPLICATES
# ============================================================================

cat("========================================\n")
cat("STEP 5: Identifying anomalies\n")
cat("========================================\n\n")

# Check for duplicates in CCAvenue data
ccavenue_duplicates <- ccavenue_clean %>%
  group_by(CCAvenue_Ref) %>%
  filter(n() > 1) %>%
  arrange(CCAvenue_Ref) %>%
  mutate(Anomaly_Type = "DUPLICATE_CCAVENUE_REF")

# Check for duplicates in CRM data
crm_duplicates <- crm_clean %>%
  group_by(Receipt_No) %>%
  filter(n() > 1) %>%
  arrange(Receipt_No) %>%
  mutate(Anomaly_Type = "DUPLICATE_RECEIPT_IN_CRM")

# Check for duplicate Payment Numbers in CRM
crm_payment_duplicates <- crm_clean %>%
  group_by(Payment_Nbr) %>%
  filter(n() > 1) %>%
  arrange(Payment_Nbr) %>%
  mutate(Anomaly_Type = "DUPLICATE_PAYMENT_NBR")

# Transactions in CCAvenue but not in CRM
missing_in_crm <- ccavenue_matched %>%
  filter(Match_Status == "NOT_IN_CRM")

# Payments in CRM but not in CCAvenue
missing_in_ccavenue <- crm_matched %>%
  filter(Match_Status == "NOT_IN_CCAVENUE")

# Amount mismatches
amount_mismatches <- crm_matched %>%
  filter(Match_Status == "MATCHED" & Amount_Match == "MISMATCH")

# Entity code mismatches
entity_mismatches <- crm_matched %>%
  filter(Match_Status == "MATCHED" & Entity_Code_Match == "MISMATCH")

# ============================================================================
# 6. SUMMARY STATISTICS
# ============================================================================

cat("\n========================================\n")
cat("STEP 6: VALIDATION SUMMARY\n")
cat("========================================\n")
cat("Total CCAvenue transactions:", nrow(ccavenue_clean), "\n")
cat("Total CRM online payments:", nrow(crm_clean), "\n")
cat("Matched records:", sum(crm_matched$Match_Status == "MATCHED"), "\n")
cat("\nANOMALIES FOUND:\n")
cat("  - Duplicate CCAvenue Ref#:", nrow(ccavenue_duplicates), "\n")
cat("  - Duplicate Receipt# in CRM:", nrow(crm_duplicates), "\n")
cat("  - Duplicate Payment Nbr# in CRM:", nrow(crm_payment_duplicates), "\n")
cat("  - CCAvenue transactions not in CRM:", nrow(missing_in_crm), "\n")
cat("  - CRM payments not in CCAvenue:", nrow(missing_in_ccavenue), "\n")
cat("  - Amount mismatches:", nrow(amount_mismatches), "\n")
cat("  - Entity code mismatches:", nrow(entity_mismatches), "\n")
cat("\nAMOUNT ANALYSIS:\n")
cat("  - Total CCAvenue amount: ₹",
    format(sum(ccavenue_clean$Order_Amount, na.rm = TRUE), big.mark = ","), "\n")
cat("  - Total CRM online amount: ₹",
    format(sum(crm_clean$Amount_CRM, na.rm = TRUE), big.mark = ","), "\n")
cat("  - Matched amount (CRM): ₹",
    format(sum(crm_matched$Amount_CRM[crm_matched$Match_Status == "MATCHED"], na.rm = TRUE),
           big.mark = ","), "\n")
cat("========================================\n")

# ============================================================================
# 7. CREATE OUTPUT FILES
# ============================================================================

cat("\n========================================\n")
cat("STEP 7: Creating output files\n")
cat("========================================\n\n")

# Add anomaly flags to main datasets
crm_matched_final <- crm_matched %>%
  mutate(
    Anomaly_Flags = case_when(
      Match_Status == "NOT_IN_CCAVENUE" ~ "❌ NOT_IN_VENDOR",
      Amount_Match == "MISMATCH" ~ "⚠️ AMOUNT_MISMATCH",
      Entity_Code_Match == "MISMATCH" ~ "⚠️ ENTITY_MISMATCH",
      TRUE ~ "✓ OK"
    )
  ) %>%
  arrange(desc(Match_Status != "MATCHED"), Payment_Date)

ccavenue_matched_final <- ccavenue_matched %>%
  mutate(
    Anomaly_Flags = case_when(
      Match_Status == "NOT_IN_CRM" ~ "❌ NOT_IN_CRM",
      Amount_Match == "MISMATCH" ~ "⚠️ AMOUNT_MISMATCH",
      Entity_Code_Match == "MISMATCH" ~ "⚠️ ENTITY_MISMATCH",
      TRUE ~ "✓ OK"
    )
  ) %>%
  arrange(desc(Match_Status != "MATCHED"), Order_Datetime)

# Create comprehensive Excel output
output_list <- list(
  "CRM_With_Vendor_Data" = crm_matched_final,
  "Vendor_With_CRM_Data" = ccavenue_matched_final,
  "Missing_In_CRM" = missing_in_crm,
  "Missing_In_Vendor" = missing_in_ccavenue,
  "Amount_Mismatches" = amount_mismatches,
  "Entity_Mismatches" = entity_mismatches,
  "Duplicate_CCAvenue_Ref" = ccavenue_duplicates,
  "Duplicate_CRM_Receipt" = crm_duplicates,
  "Duplicate_Payment_Nbr" = crm_payment_duplicates,
  "Summary_Stats" = data.frame(
    Metric = c("Total CCAvenue Transactions",
               "Total CRM Online Payments",
               "Matched Records",
               "Missing in CRM",
               "Missing in CCAvenue",
               "Amount Mismatches",
               "Entity Code Mismatches",
               "Duplicate CCAvenue Refs",
               "Duplicate CRM Receipts",
               "Total CCAvenue Amount",
               "Total CRM Amount",
               "Matched Amount"),
    Value = c(nrow(ccavenue_clean),
              nrow(crm_clean),
              sum(crm_matched$Match_Status == "MATCHED"),
              nrow(missing_in_crm),
              nrow(missing_in_ccavenue),
              nrow(amount_mismatches),
              nrow(entity_mismatches),
              nrow(ccavenue_duplicates),
              nrow(crm_duplicates),
              sum(ccavenue_clean$Order_Amount, na.rm = TRUE),
              sum(crm_clean$Amount_CRM, na.rm = TRUE),
              sum(crm_matched$Amount_CRM[crm_matched$Match_Status == "MATCHED"],
                  na.rm = TRUE))
  )
)

# Write to Excel
output_file <- file.path(output_dir,
                         paste0("Payment_Validation_Report_",
                                format(Sys.Date(), "%Y%m%d"), ".xlsx"))
write_xlsx(output_list, output_file)

cat("✓ Excel report created:", basename(output_file), "\n")
cat("  Location:", output_file, "\n\n")

# Also create CSV outputs for the two main enriched datasets
crm_csv_file <- file.path(output_dir,
                          paste0("CRM_Enriched_With_Vendor_",
                                 format(Sys.Date(), "%Y%m%d"), ".csv"))
write_csv(crm_matched_final, crm_csv_file)
cat("✓ CRM enriched CSV created:", basename(crm_csv_file), "\n")

vendor_csv_file <- file.path(output_dir,
                             paste0("Vendor_Enriched_With_CRM_",
                                    format(Sys.Date(), "%Y%m%d"), ".csv"))
write_csv(ccavenue_matched_final, vendor_csv_file)
cat("✓ Vendor enriched CSV created:", basename(vendor_csv_file), "\n\n")

# ============================================================================
# 8. DETAILED ANALYSIS (Optional)
# ============================================================================

cat("========================================\n")
cat("STEP 8: Additional analysis\n")
cat("========================================\n\n")

# Payment mode distribution in matched vs unmatched
payment_mode_analysis <- crm_matched %>%
  group_by(Pay_Mode, Match_Status) %>%
  summarise(
    Count = n(),
    Total_Amount = sum(Amount_CRM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Pay_Mode, Match_Status)

cat("Payment Mode Analysis:\n")
print(payment_mode_analysis)
cat("\n")

# Entity-wise anomaly count
entity_anomaly_summary <- crm_matched %>%
  filter(Match_Status != "MATCHED" |
         Amount_Match == "MISMATCH" |
         Entity_Code_Match == "MISMATCH") %>%
  group_by(Entity_Code_CRM, Entity_Name_CRM) %>%
  summarise(
    Anomaly_Count = n(),
    Total_Amount = sum(Amount_CRM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Anomaly_Count))

if(nrow(entity_anomaly_summary) > 0) {
  cat("Top 10 Entities with Anomalies:\n")
  print(head(entity_anomaly_summary, 10))
  cat("\n")

  # Add to output file
  output_list$Entity_Anomaly_Summary <- entity_anomaly_summary
  write_xlsx(output_list, output_file)
}

# Date-wise transaction analysis
date_analysis <- crm_matched %>%
  mutate(Payment_Month = floor_date(mdy_hms(Payment_Date), "month")) %>%
  group_by(Payment_Month, Match_Status) %>%
  summarise(
    Count = n(),
    Total_Amount = sum(Amount_CRM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Payment_Month, Match_Status)

cat("Month-wise Analysis:\n")
print(date_analysis)
cat("\n")

cat("========================================\n")
cat("✓ VALIDATION COMPLETE!\n")
cat("========================================\n\n")
cat("Output files saved to:\n")
cat("  📊", basename(output_file), "\n")
cat("  📄", basename(crm_csv_file), "\n")
cat("  📄", basename(vendor_csv_file), "\n\n")
cat("Location:", output_dir, "\n\n")
cat("Please review the Excel file for detailed results and anomaly analysis.\n")
cat("========================================\n")
