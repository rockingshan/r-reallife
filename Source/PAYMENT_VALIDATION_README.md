# Online Payment Validation - Documentation

## Overview
This script validates and maps CCAvenue online transaction data with CRM payment records to identify discrepancies, duplicates, and ensure data integrity.

## Data Sources

### 1. CCAvenue Transaction Data (`apr-dec.csv`)
**Key Fields:**
- `CCAvenue Ref#` - Unique transaction reference (PRIMARY KEY for matching)
- `Order No` - Vendor order number
- `Merchant Param1` - Entity Code (LCO code)
- `Order Amount` - Transaction amount
- `Order Status` - Transaction status
- `Payment Mode` - Payment method used

### 2. CRM Payment Data (`16823409_ListofPaymentsReport_APR-DEC25.CSV`)
**Key Fields:**
- `Receipt#` - CCAvenue reference number (FOREIGN KEY for matching)
- `Payment Nbr#` - CRM payment number
- `Entity Code` - LCO code (should match Merchant Param1)
- `Amount` - Payment amount
- `Pay Mode` - Payment type (filtered for "ONLINE PAYMENT" and "Online Transfer")
- `Reference Number` - Order number from vendor

## Matching Logic

### Primary Match Key
**Receipt# (CRM) = CCAvenue Ref# (Vendor)**

### Secondary Validations
1. **Amount Match**: CRM Amount vs CCAvenue Order Amount
2. **Entity Code Match**: Entity Code (CRM) vs Merchant Param1 (Vendor)
3. **Reference Match**: Reference Number (CRM) vs Order No (Vendor)

## Output Files

### 1. Excel Workbook: `Payment_Validation_Report_YYYYMMDD.xlsx`

**Sheet 1: CRM_With_Vendor_Data**
- All CRM online payments enriched with CCAvenue transaction details
- Shows: Payment Nbr#, Order No, Transaction details, Bank reference
- Anomaly flags for each record

**Sheet 2: Vendor_With_CRM_Data**
- All CCAvenue transactions enriched with CRM payment details
- Shows: CCAvenue Ref#, CRM Payment Nbr#, Entity details
- Anomaly flags for each record

**Sheet 3: Missing_In_CRM**
- CCAvenue transactions that don't have corresponding CRM payment records
- **Action Required**: Investigate why these transactions are not in CRM

**Sheet 4: Missing_In_Vendor**
- CRM online payments that don't have corresponding CCAvenue transactions
- **Action Required**: Verify if these were manual entries or data errors

**Sheet 5: Amount_Mismatches**
- Records where CRM amount ≠ CCAvenue amount
- Shows amount difference for investigation

**Sheet 6: Entity_Mismatches**
- Records where Entity Code doesn't match between systems
- Could indicate data entry errors

**Sheet 7: Duplicate_CCAvenue_Ref**
- Duplicate CCAvenue reference numbers in vendor data
- **Critical**: Should be investigated immediately

**Sheet 8: Duplicate_CRM_Receipt**
- Duplicate receipt numbers in CRM data
- **Critical**: Should be investigated immediately

**Sheet 9: Duplicate_Payment_Nbr**
- Duplicate payment numbers in CRM
- **Critical**: Should be investigated immediately

**Sheet 10: Summary_Stats**
- High-level statistics and totals
- Quick reference for validation status

**Sheet 11: Entity_Anomaly_Summary** (if anomalies exist)
- Entity-wise breakdown of anomalies
- Helps identify problematic LCOs

### 2. CSV Files

**CRM_Enriched_With_Vendor_YYYYMMDD.csv**
- Detailed CRM data with vendor information
- Use for importing back into CRM or analysis

**Vendor_Enriched_With_CRM_YYYYMMDD.csv**
- Detailed vendor data with CRM information
- Use for reconciliation with vendor

## Anomaly Flags

| Flag | Meaning | Action Required |
|------|---------|----------------|
| ✓ OK | Perfect match, no issues | None |
| ❌ NOT_IN_VENDOR | CRM payment has no CCAvenue transaction | Investigate missing transaction |
| ❌ NOT_IN_CRM | CCAvenue transaction has no CRM payment | Add to CRM or investigate |
| ⚠️ AMOUNT_MISMATCH | Amounts don't match between systems | Verify correct amount |
| ⚠️ ENTITY_MISMATCH | Entity codes don't match | Check data entry |

## How to Run

### Prerequisites
Install required R packages:
```r
install.packages(c("dplyr", "readr", "tidyr", "stringr", "writexl", "lubridate"))
```

### Execution Steps

1. **Open R or RStudio**

2. **Run the script:**
```r
source("validate_online_payments.R")
```

3. **Follow the interactive prompts:**

   **PROMPT 1: Select CCAvenue Transaction File**
   - A Windows file browser will open
   - Navigate to and select your CCAvenue vendor file
   - Example filename: `apr-dec.csv`
   - This file contains: CCAvenue Ref#, Order No, Merchant Param1, Order Amount

   **PROMPT 2: Select CRM Payment Report File**
   - A Windows file browser will open again
   - Navigate to and select your CRM payment report
   - Example filename: `16823409_ListofPaymentsReport_APR-DEC25.CSV`
   - This file contains: Receipt#, Payment Nbr#, Entity Code, Amount, Pay Mode

   **PROMPT 3: Select Output Directory**
   - A folder browser will open
   - Choose where you want to save the validation results
   - Default: Same folder as the CCAvenue file

4. **Wait for processing**
   - The script will load, clean, match, and analyze the data
   - Progress messages will be displayed in the console

5. **Review output files**
   - Check the selected output directory for results
   - Three files will be created (see Output Files section below)

## Expected Console Output

The script will display:
1. Number of records loaded from each source
2. Number of filtered online payments from CRM
3. Match statistics
4. Anomaly counts
5. Total amount reconciliation
6. Payment mode analysis
7. Month-wise analysis

## Key Validation Checks

### 1. Completeness Check
- Are all CCAvenue transactions recorded in CRM?
- Are all CRM online payments from CCAvenue?

### 2. Accuracy Check
- Do amounts match exactly?
- Are entity codes consistent?

### 3. Duplicate Check
- Any duplicate reference numbers?
- Any duplicate payment numbers?

### 4. Consistency Check
- Reference numbers matching between systems?
- Entity names consistent?

## Common Issues & Solutions

### Issue: High number of "Missing in CRM"
**Possible Causes:**
- Transactions not yet entered in CRM
- Failed transactions not filtered out
- Date range mismatch

**Solution:**
- Check Order Status in CCAvenue data
- Verify date ranges match
- Review failed/cancelled transactions

### Issue: Amount Mismatches
**Possible Causes:**
- Manual adjustments in CRM
- Partial payments
- Data entry errors

**Solution:**
- Review transaction history
- Check for adjustments/refunds
- Verify with original receipts

### Issue: Duplicates
**Possible Causes:**
- Double entry in CRM
- Resubmitted transactions
- System errors

**Solution:**
- Review timestamps
- Check with LCOs
- Investigate system logs

## Additional Analysis

The script also provides:
- **Payment Mode Distribution**: Breakdown by payment method
- **Entity-wise Anomaly Summary**: Which LCOs have most issues
- **Month-wise Trends**: Temporal analysis of matches/mismatches

## Data Quality Metrics

After running, you should have:
- Match Rate: (Matched / Total CRM Online) × 100
- Amount Accuracy: (Exact Matches / Matched) × 100
- Entity Accuracy: (Entity Matches / Matched) × 100
- Duplicate Rate: (Duplicates / Total) × 100

## Recommendations

1. **Daily/Weekly Run**: Run this validation regularly to catch issues early
2. **Review Anomalies**: Prioritize resolving duplicates and missing records
3. **Entity Training**: Entities with frequent mismatches may need training
4. **Process Improvement**: Use trends to identify systematic issues

## Contact & Support

For issues or questions about the validation process, review the console output and Excel sheets for detailed information.

---
*Generated on: 2026-01-09*
*Version: 1.0*
