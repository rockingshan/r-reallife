# Quick Start Guide - Online Payment Validation

## 🚀 How to Use This Script

### Step 1: Install Required Packages (One-time setup)
Open R or RStudio and run:
```r
install.packages(c("dplyr", "readr", "tidyr", "stringr", "writexl", "lubridate"))
```

### Step 2: Run the Script
```r
source("validate_online_payments.R")
```

### Step 3: Select Files (3 Dialog Boxes)

#### Dialog 1️⃣: Select CCAvenue Transaction File
```
Please select the CCAvenue ONLINE TRANSACTION VENDOR file
Example: apr-dec.csv
```
**What to select:** The file from CCAvenue containing online transaction data
**Key columns:** CCAvenue Ref#, Order No, Merchant Param1, Order Amount

---

#### Dialog 2️⃣: Select CRM Payment Report
```
Please select the CRM PAYMENT REPORT file
Example: 16823409_ListofPaymentsReport_APR-DEC25.CSV
```
**What to select:** The payment report exported from your CRM system
**Key columns:** Receipt#, Payment Nbr#, Entity Code, Amount, Pay Mode

---

#### Dialog 3️⃣: Choose Output Folder
```
Please select the OUTPUT DIRECTORY where results will be saved
```
**What to select:** Folder where you want the validation results saved
**Default:** If you cancel, it will use the same folder as the CCAvenue file

---

### Step 4: Wait for Processing
The script will:
- ✓ Load both data files
- ✓ Filter CRM data for online payments only
- ✓ Match records between systems
- ✓ Identify anomalies and duplicates
- ✓ Generate detailed reports

### Step 5: Review Output Files

Three files will be created in your output directory:

📊 **Payment_Validation_Report_YYYYMMDD.xlsx**
- Main Excel report with 11 sheets
- All analysis, anomalies, and statistics

📄 **CRM_Enriched_With_Vendor_YYYYMMDD.csv**
- CRM payment data + CCAvenue transaction details

📄 **Vendor_Enriched_With_CRM_YYYYMMDD.csv**
- CCAvenue transaction data + CRM payment details

---

## 📋 What the Script Validates

✅ **All CCAvenue transactions exist in CRM?**
✅ **All CRM online payments have CCAvenue transactions?**
✅ **Amounts match between systems?**
✅ **Entity codes consistent?**
✅ **No duplicate transactions?**
✅ **Total amounts reconcile?**

---

## 🚨 Key Anomalies to Review

| Priority | Anomaly | Sheet Name |
|----------|---------|------------|
| 🔴 HIGH | Duplicate Transactions | Duplicate_CCAvenue_Ref, Duplicate_CRM_Receipt |
| 🟠 MEDIUM | Missing in CRM | Missing_In_CRM |
| 🟠 MEDIUM | Missing in Vendor | Missing_In_Vendor |
| 🟡 LOW | Amount Mismatches | Amount_Mismatches |
| 🟡 LOW | Entity Mismatches | Entity_Mismatches |

---

## 💡 Console Output Example

```
========================================
ONLINE PAYMENT VALIDATION TOOL
========================================

STEP 1: Select data files
---------------------------

Please select the CCAvenue ONLINE TRANSACTION VENDOR file
(Example: apr-dec.csv)
...
Selected: C:/Users/.../apr-dec.csv

Please select the CRM PAYMENT REPORT file
...
Selected: C:/Users/.../16823409_ListofPaymentsReport_APR-DEC25.CSV

========================================
STEP 2: Loading data files
========================================

✓ CCAvenue records loaded: 59137
✓ CRM records loaded: 72208

========================================
STEP 3: Cleaning and preparing data
========================================

✓ Filtered CRM online payments: XXXXX

========================================
STEP 4: Matching records between systems
========================================

========================================
STEP 5: Identifying anomalies
========================================

========================================
STEP 6: VALIDATION SUMMARY
========================================
Total CCAvenue transactions: 59137
Total CRM online payments: XXXXX
Matched records: XXXXX

ANOMALIES FOUND:
  - CCAvenue transactions not in CRM: XXX
  - CRM payments not in CCAvenue: XXX
  - Amount mismatches: XXX
  - Entity code mismatches: XXX
  ...

========================================
✓ VALIDATION COMPLETE!
========================================
```

---

## 🔍 Quick Troubleshooting

### Error: "could not find function"
**Solution:** Install missing packages (see Step 1)

### Error: "file does not exist"
**Solution:** Make sure you selected the correct file in the dialog

### Warning: "NA values"
**Solution:** This is normal - some fields may be empty in the source data

### No anomalies found
**Solution:** Great! Your data is perfectly synchronized 🎉

---

## 📞 Need Help?

Review the detailed documentation in: `PAYMENT_VALIDATION_README.md`

---

**Last Updated:** 2026-01-09
