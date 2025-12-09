# R Scripts Index - Complete Reference Guide

**Last Updated:** 2025-12-09
**Total Scripts:** 59 (9 root + 25 monthly + 25 tools)

---

## Table of Contents
1. [Quick Reference - Common Tasks](#quick-reference---common-tasks)
2. [Task-Based Index](#task-based-index)
3. [Complete Script Inventory](#complete-script-inventory)
4. [Code Snippet Library](#code-snippet-library)
5. [Duplication Map](#duplication-map)
6. [Input/Output Matrix](#inputoutput-matrix)

---

## Quick Reference - Common Tasks

| **I Need To...** | **Use This Script** | **Location** |
|------------------|---------------------|--------------|
| Generate monthly MSR reports | `ListActivetoMSR.R` | `Source/Monthly/` |
| Generate IPTV state-wise MSR | `IPTVMSR_v2.R` | `Source/Monthly/` |
| Analyze wallet revenue | `Wallet_analysis.R` | `Source/Monthly/` |
| Reconcile wallet balances | `Wallet_reconcile.R` | `Source/Monthly/` |
| Get broadcaster reports | `BC_Report.R` | `Source/Monthly/` |
| Reconcile CAS with MQ data | `Reconcilliation.R` or `Gospell_command_eval.R` | `Source/Monthly/` or `Source/Tools/` |
| Calculate LCO payments | `LcoPaymentCalc.R` | `Source/Tools/` |
| Analyze active customers | `active_analysis.R` | `Source/Monthly/` |
| Track subscriber trends | `Subscriber_Trend_analysis.R` | `Source/Tools/` |
| Forecast subscriber counts | `Forecasting.R` | `Source/` |
| Calculate refunds | `refund_find.R` | `Source/` |
| Get LCO active counts | `new_lco_active.R` | `Source/` |
| Track disconnected aging | `Disconnected_aging.R` | `Source/Tools/` |
| Send retrack commands | `retrack_startToday.R` | `Source/` |
| Find code for specific task | See [Code Snippet Library](#code-snippet-library) below |

---

## Task-Based Index

### 1. MONTHLY REPORTING

#### MSR (Monthly Subscription Report) Generation
- **Cable MSR**: `Source/Monthly/ListActivetoMSR.R`
  - Converts MQ active list to broadcaster settlement format
  - Tracks 7th, 14th, 21st, 28th day snapshots
  - Calculates monthly averages
  - Outputs: Multi-sheet Excel reports

- **IPTV MSR**: `Source/Monthly/IPTVMSR_v2.R`
  - State-wise IPTV reporting (WB, UP, Bihar)
  - HD/SD segmentation
  - Outputs: State-wise Excel + formatted Word docs

- **Area-wise MSR**: `Source/Monthly/AreawiseMSR.R`
  - Area and LCO-wise breakdowns
  - 4-week snapshot aggregation

- **Broadcaster-Specific MSR**:
  - `Source/Monthly/BC_Report_ZEE.R` - Zee specific
  - `Source/Monthly/BC_report_areawise.R` - Area-wise broadcaster reports

#### Wallet & Revenue Analysis
- **Main Wallet Analysis**: `Source/Monthly/Wallet_analysis.R`
  - Area-wise exports (Berhampore, Haldia)
  - LCO-wise revenue summaries
  - Plan-wise and service-wise breakdowns
  - Broadcaster revenue share calculations
  - Outputs: Multiple CSVs + ZIP archives

- **Wallet Reconciliation**: `Source/Monthly/Wallet_reconcile.R`
  - Complete balance reconciliation
  - Opening + Payments + Credits - Debits - Consumption
  - Variance analysis
  - Customer segmentation (paid vs promotional)

- **Broadcaster Wallet**: `Source/Monthly/Broadcaster_wallet.R`
  - Base plan-wise broadcaster revenue
  - City-wise revenue analysis
  - Revenue share calculations

#### Active Customer Reports
- **Active Analysis**: `Source/Monthly/active_analysis.R`
  - Comprehensive active customer analysis
  - MSR vs List Active reconciliation
  - Broadcaster-wise service counts
  - CAS entitlement validation
  - LCO-wise plan counts

- **Active Customer Count**: `Source/Monthly/ActiveCustomer.R`
  - LCO-wise active counts
  - Wallet validation (dual method)

- **Penetration Analysis**: `Source/Monthly/Penetration_Percentage.R`
  - Area-wise plan penetration
  - Channel penetration percentages
  - Multi-sheet Excel output

#### Broadcaster Reports
- **Main Broadcaster Report**: `Source/Monthly/BC_Report.R`
  - Plan-wise broadcaster breakdown
  - MSR reconciliation
  - DPO vs old plan comparison
  - Penetration calculations

### 2. RECONCILIATION & VALIDATION

#### CAS System Reconciliation
- **All Systems**: `Source/Monthly/Reconcilliation.R`
  - Gospell, SafeView, ABV, Nagra
  - Identifies CAS entries missing in MQ
  - Identifies MQ entries missing in CAS

- **Gospell-Specific**: `Source/Tools/Gospell_command_eval.R`
  - Detailed Gospell reconciliation
  - Inactive card identification
  - Area-wise export

#### Billing Validation
- **Service Without Wallet**: `Source/Monthly/Find_Service_no_wallet.R`
  - Finds active services without proper billing
  - Validates billing period
  - Latest transaction matching

- **Wallet vs Active**: `Source/Monthly/Wallet_reconcile.R`
  - See above under Wallet & Revenue

### 3. CUSTOMER OPERATIONS

#### Disconnections & Renewals
- **Retrack Commands**:
  - `Source/retrack_startToday.R` - For contracts ending today
  - `Source/retrack_startTomorrow.R` - For contracts ending tomorrow
  - `Source/retrack_endToday.R` - For contracts ending today
  - Provisions STBs via MQ API

- **Disconnect/Reconnect**: `Source/resendBose_startTomorrow.R`
  - Vacation mode operations
  - Two-step API operations

- **Disconnected Aging**: `Source/Tools/Disconnected_aging.R`
  - Analyzes time to renewal
  - Identifies non-renewed customers
  - Aging distribution

#### Refunds & Adjustments
- **Refund Calculation**: `Source/refund_find.R`
  - Pro-rata refund amounts
  - Matches wallet charges with refund dates
  - Identifies unmatched records

### 4. INVENTORY & HARDWARE

#### LCO Active & Inventory
- **LCO Active Data**: `Source/new_lco_active.R`
  - Active customer vs STB inventory reconciliation
  - Capacity utilization (Active/STB percentage)
  - Special handling for Pragati LCO

- **Blank Hardware Assignment**: `Source/Monthly/FInd_nopair_box.R`
  - Identifies discontinued STB-VC pairs
  - Greedy algorithm for assigning blank customers
  - Entity-aware matching

- **Stock Reports**: `Source/Tools/stock_report.R`
  - STB/SC inventory reports

### 5. ANALYSIS & FORECASTING

#### Trend Analysis
- **Subscriber Trends**: `Source/Tools/Subscriber_Trend_analysis.R`
  - Visualizes trends over time
  - Migration patterns
  - Contract change patterns
  - Package type trends
  - Outputs: PNG plots

- **Forecasting**: `Source/Forecasting.R`
  - Prophet forecasting
  - Holt's exponential smoothing
  - ARIMA models
  - Accuracy metrics (MAE, RMSE, MAPE)

#### Historical Analysis
- **Customer Historical**: `Source/Tools/cust_historical.R`
  - Historical customer journey
  - STB distribution analysis

- **LCO Historical**: `Source/Tools/historical.R`
  - STB type distribution
  - City-wise LCO organization

### 6. DAILY OPERATIONS

#### User Tracking
- **Daily Work**: `Source/Tools/DailyWork.R`
  - SMS user activity tracking
  - Transaction type counts
  - User performance reports

#### Payment Processing
- **LCO Payment Calculation**: `Source/Tools/LcoPaymentCalc.R`
  - Pro-rata payment requirements
  - Tax-inclusive amounts
  - LCO-specific pricing lookup

- **Online Payment Reconciliation**: `Source/Tools/OnlinePaymentRecon.R`
  - HDFC payment reconciliation

### 7. CONFIGURATION & UTILITIES

#### Data Processing
- **Functions Library**: `Source/Functions.R`
  - MQ active list import
  - CAS system identification
  - SMS API integration

- **Merge Utilities**: `Source/Tools/MERGE_CREATE.R`
  - Data source merging

- **CAS Data Creation**: `Source/Monthly/CAS_data_make.R`
  - Multi-CAS system data extraction
  - Safeview and ABV processing

#### Package Management
- **Package Creation**: `Source/Tools/create_package.R`
  - Package configuration

- **DPO to Broadcaster**: `Source/Tools/DPOpacktoBroadcaster.R`
  - DPO package mapping

- **Package Costing**: `Source/Tools/PackageCosting.R`
  - Cost calculations

#### Customer Broadcaster Mapping
- **Service Mapping**: `Source/Monthly/Customer_Broadcsater_Service.R`
  - Customer-broadcaster matrix
  - Service count per broadcaster

### 8. TESTING & DEVELOPMENT

- **Test Bench**: `Source/testbench.R`
  - Ad-hoc analysis playground
  - 20+ different analysis patterns
  - Experimentation workspace

- **Shiny Test**: `Source/shiny_test.R`
  - Interactive web UI testing
  - Inventory processing

---

## Complete Script Inventory

### ROOT SCRIPTS (Source/)

#### Functions.R
**Size:** 2.8 KB
**Purpose:** Central utility library for common operations

**Key Functions:**
1. `mq_active_report()` - Imports and standardizes MQ active customer list
2. CAS system identification by smart card length (8=Gospell, 12=Safeview, 15=Nagra, 16=ABV)
3. SMS notification sender via RapidSMS API

**Inputs:** MQ active list CSV, mobile numbers
**Outputs:** Standardized dataframes
**Dependencies:** tidyverse, dplyr, readxl, lubridate, httr, xlsx

**Key Code:**
```r
# CAS system ID by card length
VC.length <- nchar(as.character(df$SMART_CARD_NBR))
VC.length <- gsub("8","GOSPELL",gsub("12","SAFEVIEW",gsub("15","NAGRA",gsub("16","ABV",VC.length))))
```

---

#### Forecasting.R
**Size:** 3.7 KB
**Purpose:** Time series forecasting with multiple methodologies

**Key Functions:**
1. Simple exponential smoothing
2. Linear regression for trends
3. Holt's method (alpha/beta tuning)
4. Facebook Prophet with seasonal decomposition
5. Model accuracy testing (train-test split, MAE, RMSE, MAPE)

**Inputs:** Monthly customer count data (YYYY-MM format)
**Outputs:** Forecast plots and predicted values
**Dependencies:** readr, forecast, ggplot2, prophet, Metrics

**Key Code:**
```r
# Time series creation
ts_data <- ts(data$Customers, start=c(2023,12), frequency=12)

# Prophet formatting
prophet_df <- data %>% rename(ds=Month, y=Customers)

# Accuracy metrics
mae(actual, predicted)
```

---

#### new_lco_active.R
**Size:** 3.3 KB
**Purpose:** Calculate active customer counts vs STB inventory by LCO

**Key Functions:**
1. Loads MQ active list and filters by entity
2. Reads inventory and counts STBs (excludes smart cards)
3. Calculates active percentage (Active/STB ratio)
4. Special handling for Pragati LCO (MD0305)
5. LCO-wise summary with totals

**Inputs:** MQ active list, inventory file, LCO master, Pragati VC list
**Outputs:** `New_LCO_data_DDMMYYYY.CSV`
**Dependencies:** tidyverse, dplyr, lubridate, janitor

**Key Code:**
```r
# STB count from inventory
filter(!(str_detect(ITEM_CODE, "SC"))) %>%
filter(!(str_detect(ITEM_CODE, "NAGRA"))) %>%
group_by(ENTITY_CODE) %>%
summarise(Total_STB = n())

# Percentage calculation
mutate(Percent_Active = paste0(round(Total_Active/Total_STB*100,2),"%"))
```

---

#### refund_find.R
**Size:** 2.0 KB
**Purpose:** Calculate pro-rata refunds for disconnected customers

**Key Functions:**
1. Reads historical transactions and wallet report
2. Identifies refund transactions
3. Matches wallet charges with refund dates
4. Calculates pro-rata credit: (Amount/Frequency)*UnusedDays
5. Exports unmatched records

**Inputs:** Historical transaction CSV, wallet report
**Outputs:** `credit_fcn.csv`, `_fcn_nafile.csv`, `FCN_WALLET.CSV`
**Dependencies:** tidyverse, dplyr, lubridate, readxl

**Key Code:**
```r
# Pro-rata calculation
mutate(credit_amnt = (Amount.Debit/Billing.Frequency) *
       as.numeric((Billing.Frequency-(Transaction.Date.y-Transaction.Date.x))))
```

---

#### resendBose_startTomorrow.R
**Size:** ~3 KB
**Purpose:** Send disconnect/reconnect commands for vacation mode

**Key Functions:**
1. Reads due for renewal list
2. Filters by entity, CAS system, end date (today+30)
3. Creates XML request bodies for disconnect and reconnect
4. Makes HTTP POST to MQ BCRM API
5. Handles authentication and reference number generation

**Inputs:** Due for renewal list
**Outputs:** Console API responses
**Dependencies:** tidyverse, dplyr, lubridate, httr

**Key Code:**
```r
# XML body construction
body_xml <- paste0("<REQUESTINFO>\r\n<DISCONNECTIONINFO>\r\n<DISCONNECTIONDATE>",
                   mqdate,"</DISCONNECTIONDATE>\r\n...")

# API headers
headers <- c('USERNAME'='MB102', 'PASSWORD'='****', 'EXTERNALPARTY'='MQS')

# Reference number
ref_num <- paste0(format(date, "%d%m%Y%H%M%S"), "ABRTEY")
```

---

#### retrack_*.R (3 variants)
**Variants:** `retrack_startToday.R`, `retrack_startTomorrow.R`, `retrack_endToday.R`
**Purpose:** Send RETRACK commands to provision STBs via MQ API

**Key Functions:**
1. Reads due for renewal list
2. Filters by CAS system (ABV/Gospell) and contract end date
3. Creates XML RETRACK request
4. Submits HTTP POST with provisioning parameters
5. Iterates with timing delays

**Inputs:** Due for renewal list with Smart.Card.Number, Customer.Number
**Outputs:** API response messages
**Dependencies:** tidyverse, dplyr, lubridate, httr

**Differences:**
- `startToday`: Filters for `Contract.End.Date == today()`
- `startTomorrow`: Filters for `today()+29` or `today()+30`
- `endToday`: Filters for `Contract.End.Date == today()`

**Key Code:**
```r
# RETRACK XML
body <- paste0("<RETRACK>\r\n<CUSTOMERNUMBER>", custListSlct[i, "Customer.Number"],
               "\r\n<HARDWAREREFNUMBER>", custListSlct[i, "Smart.Card.Number"], ...)

# Progress tracking
cat(i); cat("/"); cat(nrow(custListSlct))
```

---

#### shiny_test.R
**Size:** 1.4 KB
**Purpose:** Interactive Shiny app for inventory processing

**Key Functions:**
1. File upload UI
2. Filters inventory (excludes smart cards)
3. Saves processed file with "_STB_SERIALS.csv" suffix

**Inputs:** Inventory CSV
**Outputs:** Processed CSV in same directory
**Dependencies:** shiny, dplyr

---

#### testbench.R
**Size:** 42.4 KB
**Purpose:** Comprehensive ad-hoc analysis and testing workspace

**Contains 20+ Analysis Patterns:**
1. Daily disconnection summary by area
2. Customers without basic package in recharge
3. HD/SD box inventory counting
4. Single-row LCO active reports
5. City mapping validation
6. Plan-wise service counts
7. MSR vs List Active entitlement reconciliation
8. Package creator (multi-CAS)
9. Alacarte and Bouquet reconciliation
10. Customer activation history
11. CSV to Excel with leading quote
12. Historical vs active list comparison
13. Gospel inactive cards identification
14. Disconnected aging slabs
15. MSR to active tally
16. Blank inventory assignment
17. Authority renewal tracking
18. New plan tracking and old package removal via API
19. Email sending for LCO communications
20. Alacarte customer finder by broadcaster
21. Blank STB/VC pairing
22. XML EPG parsing
23. Transaction hour analysis
24. Plan name standardization

**Inputs:** Multiple data sources
**Outputs:** 20+ CSV files
**Dependencies:** tidyverse, dplyr, lubridate, janitor, httr, xlsx, jsonlite, RDCOMClient, XML, plyr, svDialogs

**Use Case:** Experimentation, one-off analyses, code pattern testing

---

### MONTHLY SCRIPTS (Source/Monthly/)

#### active_analysis.R
**Size:** 26.1 KB
**Purpose:** Comprehensive active customer analysis with multi-point reconciliation

**Key Functions:**
1. Imports MQ active list via Functions.R
2. Creates service-to-broadcaster mappings
3. Generates broadcaster-wise service counts
4. Creates pivot tables for active services/customers
5. Compares MSR bouquet/alacarte vs list active
6. Identifies missing services (in MSR but not in list)
7. Calculates plan-wise active counts
8. Validates CAS entitlements for all services
9. LCO-wise plan count reports
10. Promotional customer tracking

**Inputs:** MQ active list, package details, service details, plan-service mapping (Google Drive), MSR reports, CAS entitlements, promo plans
**Outputs:**
- `list_all.csv` - Active customers with contact info
- `Service_count_list_active.csv` - Service-wise counts
- `customer.csv` - LCO-wise active counts
- `Bouquets_in_MSR_notin_ListActive.csv` - Reconciliation
- `Alacarte_in_MSR_notin_ListActive.csv` - Reconciliation
- `Not in cas entitle report.csv` - CAS validation failures
- `PLAN_COUNT_*.CSV` - Plan-wise counts
- `LCOWISE_PLANCOUNT.CSV` - LCO plan distribution

**Dependencies:** tidyverse, dplyr, Functions.R

**Key Code:**
```r
# Broadcaster merge
merge(planservice, pack) %>% select(Service.Code, Broadcaster)

# MSR reconciliation
merge(ls_bq_com, ls_act_com, all.x=T) %>% filter(is.na(ENTITY_CODE))

# CAS validation
merge(ls_act_com, cas_ent_com, all.x=T) %>% filter(is.na(Prov.System.Name))

# Broadcaster pivot
group_by(CUSTOMER_NBR, Broadcaster) %>%
  summarize(Acc_count=n()) %>%
  pivot_wider(names_from=Broadcaster, values_from=Acc_count)
```

---

#### ActiveCustomer.R
**Size:** ~2 KB
**Purpose:** Calculate active customer counts by LCO with wallet validation

**Key Functions:**
1. Reads active customer summary by entity
2. Reads wallet transaction list
3. Extracts unique customers from wallet
4. Compares wallet customer count vs active count
5. Takes maximum of two values for accuracy

**Inputs:** Active customer count, wallet report
**Outputs:** `LCOWISE_ACTIVE_CUST_{Month}_{Year}.csv`
**Dependencies:** tidyverse, dplyr, lubridate

**Key Code:**
```r
# Dual validation
merge(active_pivot, active_wallet_count, all.x=T) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(ActiveCustomer = pmax(Total_Active, WallCount))
```

---

#### AreawiseMSR.R
**Size:** ~10 KB
**Purpose:** Generate area-wise MSR with broadcaster and plan analysis

**Key Functions:**
1. Loads bouquet and alacarte MSR reports
2. Filters by broadcaster (excludes DD, FTA, Republic TV)
3. Reads single CAS code packages
4. Merges 4-week snapshots (7th, 14th, 21st, 28th)
5. Calculates monthly averages
6. Groups by broadcaster, bouquet, week
7. Pivots for weekly progression
8. Joins with LCO area mapping

**Inputs:** Bouquet MSR Excel, Alacarte MSR Excel, single CAS code packages (4 weeks), broadcaster config, LCO area mapping
**Outputs:** Excel sheets - `Alacarte_lcowise.csv`, `Bouquet_lcowise.csv`
**Dependencies:** tidyverse, dplyr, readxl, xlsx, stringr

**Key Code:**
```r
# 4-week merge
merge(bc_odisha_nw_7_pk, bc_odisha_nw_14_pk, all=T) %>%
  merge(bc_odisha_nw_21_pk, all=T) %>%
  merge(bc_odisha_nw_28_pk, all=T)

# Average calculation
mutate(Monthly.Subs.of.the.Channel =
       rowMeans(select(bc_odisha_combo, starts_with("No.of"))))
```

---

#### BC_Report.R
**Size:** 24.5 KB
**Purpose:** Main broadcaster-wise active customer analysis and reconciliation

**Key Functions:**
1. Imports MQ active list
2. Creates broadcaster mapping from package details
3. Generates service-to-broadcaster relationships
4. Creates broadcaster-wise active customer pivots
5. Reconciles MSR data with list active
6. Identifies missing services (both directions)
7. Plan-wise active customer counts
8. DPO vs old bouquet comparison
9. Plan-wise broadcaster penetration

**Inputs:** MQ active list, package details, service details, plan config (Google), MSR reports
**Outputs:**
- `listactive_to_MSR_PLANWISE.CSV` - Plan-wise broadcaster breakdown
- `listactive_to_MSR.CSV` - Service-wise broadcaster
- Various reconciliation files
- `planOnly.csv`, `sports18.csv`
- DPO count and penetration

**Dependencies:** tidyverse, dplyr

**Key Code:**
```r
# Broadcaster merge
merge(list_active_flt, broadcaster,
      by.x="SERVICE_CODE", by.y="Service.Code", all.x=T)

# Penetration
mutate(Penetration = paste0(round(Active/act_cust_count$n*100,2),"%"))
```

---

#### BC_report_areawise.R & BC_Report_ZEE.R
**Purpose:** Area-wise and broadcaster-specific MSR with 4-week tracking

**Key Functions:**
1. Read area-wise MSR reports (Excel, skip 3 rows)
2. Filter by broadcaster
3. Handle single CAS code packages
4. Merge 4-week snapshots with averaging
5. Group by area, bouquet/channel, broadcaster
6. Weekly progression pivot tables
7. Generate broadcaster-specific reports

**Inputs:** MSR Excel reports, single CAS packages, LCO area mapping, broadcaster config
**Outputs:**
- `MSR_Report_NTO_all_*.xlsx` (Bouquet/Alacarte sheets)
- `MSR_Report_NTO_Planwise_*.xlsx`
- `Areawise_MSR_Report_all_*.xlsx`

**Dependencies:** tidyverse, dplyr, readxl, xlsx, stringr

**Key Code:**
```r
# Area column addition
add_column(Area = "Kolkata", .after = 4)

# Area-wise pivot
group_by(City, Bouquet) %>%
  summarize('Active_7th'=sum(...), 'Active_14th'=sum(...),
            'Active_21st'=sum(...), 'Active_28th'=sum(...),
            'Average'=sum(...))
```

---

#### Broadcaster_wallet.R
**Size:** ~6 KB
**Purpose:** Calculate broadcaster revenue share from wallet by base plan and geography

**Key Functions:**
1. Reads wallet, active list, package details, plan-service mapping
2. Creates service-to-broadcaster mapping
3. Filters wallet for alacarte/bouquet only (excludes base)
4. Maps customers to base plan
5. Merges with LCO city info
6. Maps broadcaster from service names
7. Calculates base plan-wise broadcaster revenue
8. City-wise, base plan-wise, broadcaster-wise revenue
9. Pivots broadcaster revenue to columns
10. Filters for important broadcasters

**Inputs:** Wallet report, active customer list, package details, plan-service config, entity-city mapping
**Outputs:**
- `baseplan_broadcaster_revenue.csv`
- `city_baseplan_broadcaster_revenue.csv`

**Dependencies:** tidyverse, dplyr, readxl, stringr, lubridate

**Key Code:**
```r
# Base plan extraction
filter(Plan.Type=="Basic") %>%
  select(Customer.Nbr, Base.Plan.Name=Plan.Name)

# Revenue pivot
pivot_wider(names_from=Broadcaster,
            values_from=Total.Revenue,
            values_fill=0)
```

---

#### CAS_data_make.R
**Size:** ~4 KB
**Purpose:** Extract and consolidate CAS system subscriber data

**Key Functions:**
1. Reads inventory with smart card serial numbers
2. Reads Safeview CAS data (subscription IDs)
3. Reads ABV CAS data (Cumulative Active SMC Report)
4. Separates left/right portions of Safeview cards
5. Merges inventory with CAS data
6. Handles multiple CAS systems
7. Outputs clean CAS data by system

**Inputs:** Inventory file, Safeview CAS CSV, ABV CAS Excel
**Outputs:** `Sfw_final.csv`, `ABV_final.csv`
**Dependencies:** tidyverse, dplyr, readxl, microbenchmark

**Key Code:**
```r
# Card number parsing
separate(SERIAL_NUMBER, into=c("leftval","rightval"), sep=10)

# ABV filtering
filter(PPCSTATUS=="Activated") %>%
  select(SMARTCARDNO, PACKAGEID)
```

---

#### Customer_Broadcsater_Service.R
**Size:** ~3 KB
**Purpose:** Create customer-broadcaster-service matrix

**Key Functions:**
1. Reads MQ active list
2. Reads service-broadcaster mapping
3. Filters active customers (non-DPO plans)
4. Creates customer-broadcaster pivot (service count)
5. Outputs customer master with plan details

**Inputs:** MQ active list, service details with broadcaster, plan names (Google)
**Outputs:**
- `customer_broadcaster.csv` - Customer-broadcaster matrix
- `customer_data.csv` - Customer master

**Dependencies:** tidyverse, dplyr

**Key Code:**
```r
# Broadcaster pivot
group_by(CUSTOMER_NBR, Broadcaster) %>%
  summarize(ServiceCount=n()) %>%
  pivot_wider(names_from=Broadcaster, values_from=ServiceCount)
```

---

#### FInd_nopair_box.R
**Size:** ~5 KB
**Purpose:** Identify discontinued STB-VC pairs and assign blank customers

**Key Functions:**
1. Reads old and new customer master files
2. Extracts unique STB-VC pairs from both
3. Identifies pairs in old data not in new (discontinued)
4. Reads inventory for item codes and entity codes
5. Joins discontinued pairs with inventory (STB and VC separately)
6. Identifies blank customers (no STB, no VC)
7. Greedy assignment of blank customers to discontinued pairs
8. Tracks which blank customers have been used

**Inputs:** Old customer CSV, new customer CSV, inventory file
**Outputs:** `Blank_box_with_blank_customer_number.csv`
**Dependencies:** tidyverse, dplyr, readxl

**Key Code:**
```r
# Discontinued pairs
anti_join(old_pairs, new_pairs, by=c("Stb","Vc"))

# Greedy assignment
for (i in seq_len(nrow(discontinued_df))) {
  match_idx <- which(!blank_customers$used & ...)
  discontinued_df$assigned_customer[i] <- blank_customers$Customer.Number[chosen_idx]
  blank_customers$used[chosen_idx] <- TRUE
}
```

---

#### Find_Service_no_wallet.R
**Size:** ~5 KB
**Purpose:** Identify active services lacking proper wallet transactions

**Key Functions:**
1. Reads LCO wallet bills and active customer list
2. Merges with due for renewal dates
3. Filters wallet for specific transactions
4. Handles missing service names (uses plan details fallback)
5. Removes provisioning logs and incomplete records
6. Groups by customer, service, contract
7. Finds latest transaction for each service
8. Identifies active services without matching transactions
9. Identifies invalid transactions (outside billing period)
10. Combines unmatched and invalid for review

**Inputs:** LCO wallet bills, active customer list, due for renewal dates
**Outputs:** `services_without_proper_transaction.csv`
**Dependencies:** tidyverse, dplyr, lubridate

**Key Code:**
```r
# Latest transaction
group_by(Customer.Nbr, Service.Name, Contract.Number) %>%
  filter(Bill.Charge.End.Date==max(Bill.Charge.End.Date))

# Invalid detection
filter(!(END_DATE >= Bill.Charge.Start.Date &
         END_DATE <= Bill.Charge.End.Date))
```

---

#### IPTVMSR.R & IPTVMSR_v2.R
**Purpose:** Generate IPTV Monthly Subscription Reports with HD/SD segmentation

**Key Functions:**
1. Reads IPTV subscription files for different states
2. Creates summary tables (unique subscribers by date)
3. Separates HD and SD subscribers
4. Calculates HD+SD combined counts
5. Derives average subscriber counts across period
6. Subscription breakdowns by plan and package
7. Uses 7th, 14th, 21st, 28th day snapshots
8. Integrates single CAS code package info

**Inputs:** IPTV Excel files (Account_No, Subscriber, STB_No, VC_Number, User_ID, dates), package code mapping, single CAS packages (4 weeks)
**Outputs:** Summary tables with HD/SD/HD+SD counts, Excel exports with dynamic date columns
**Dependencies:** readxl, dplyr, tidyr, lubridate, flextable, officer, xlsx

**Key Code:**
```r
# User count distinct
group_by(Date, Plan_Type) %>%
  summarise(User_Count=n_distinct(User_ID))

# HD/SD pivot
pivot_wider(names_from=Plan_Type,
            values_from=User_Count,
            values_fill=0)

# Average
rowwise() %>%
  mutate(Average=mean(c_across(-Type), na.rm=TRUE))
```

---

#### ListActivetoMSR.R
**Size:** 27.2 KB
**Purpose:** Convert MQ active list to MSR format for broadcaster settlements

**Key Functions:**
1. Loads MQ active customer list
2. Loads bouquet and alacarte package definitions
3. Filters on plan types (excludes promotional, deleted)
4. Counts unique subscribers on 7th, 14th, 21st, 28th days
5. Calculates monthly averages
6. Groups by: Broadcaster → Plan → Bouquet/Channel
7. Generates multi-sheet Excel reports

**Inputs:** MQ active list, bouquet definitions, alacarte definitions, plan-service mapping
**Outputs:**
- `MSR_Report_all_*.xlsx` (multiple sheets)
- `MSR_Report_Planwise_all_*.xlsx`
- `MSR_Report_Areawise_all_*.xlsx`

**Dependencies:** tidyverse, dplyr, readxl, xlsx, lubridate

**Key Code:**
```r
# 4-week snapshot merge
merge(week_7th, week_14th, all=T) %>%
  merge(week_21st, all=T) %>%
  merge(week_28th, all=T)

# Monthly average
mutate(Monthly_Avg = rowMeans(select(., starts_with("Active_"))))
```

---

#### Penetration_Percentage.R
**Size:** ~8 KB
**Purpose:** Calculate plan and channel penetration percentages

**Key Functions:**
1. Imports MQ active list with area categorization
2. Reads broadcaster config and plan names
3. Separates active customers by area and LCO
4. Filters for plan-wise active counts
5. Generates area-wise plan penetration
6. Creates service-wise penetration (alacarte)
7. Creates bouquet-wise penetration
8. Exports multi-sheet Excel
9. Identifies promotional customer counts
10. Tracks new LCO plan adoption

**Inputs:** MQ active list, package details, broadcaster config (Google), plan names (Google), bouquet names (Google), promo plans
**Outputs:**
- `Channel_penetration.xlsx` (multi-sheet: Areawiseplan, Alacarte, Bouquets)
- `NEW_LCO_PLAN_DATA.CSV`

**Dependencies:** tidyverse, dplyr, readxl, openxlsx

**Key Code:**
```r
# Area addition
add_column(Area="Kolkata", .after=4)

# Plan pivot
group_by(Area, PLAN_NAME) %>%
  summarize(Active.Cust=n())

# Multi-sheet Excel
write.xlsx(list("Areawiseplan"=ac_plan_pivot,
                "Alacarte"=list_ac_ala_bc,
                "Bouquets"=list_ac_bq_bc))
```

---

#### PMR_REPORT_MAKE_NEW.R
**Purpose:** PMR (Package Management Report) generation
**Status:** Active monthly reporting script

---

#### Reconcilliation.R
**Size:** ~15 KB
**Purpose:** Complete CAS system reconciliation with MQ data

**Key Functions:**
1. Loads inventory (STB/SC serial numbers)
2. Loads CAS-specific subscription data (all systems)
3. Matches MQ data with CAS records
4. Identifies CAS entries missing in MQ
5. Identifies MQ entries missing in CAS
6. Separates by CAS system and LCO

**Inputs:** MQ active list, inventory, Gospell CAS data, Safeview CAS data, ABV CAS data, Nagra CAS data
**Outputs:**
- `Safeview_active_service_not_in_MQ.csv`
- ABV/Gospell/Nagra reconciliation reports

**Dependencies:** tidyverse, dplyr, readxl

**Key Code:**
```r
# CAS to MQ merge
merge(cas_data, mq_data, by="smart_card", all.x=T) %>%
  filter(is.na(CUSTOMER_NBR))  # Missing in MQ
```

---

#### Wallet_analysis.R
**Size:** 6.4 KB
**Purpose:** Multi-purpose wallet transaction analysis

**Key Functions:**
1. Area-wise export (Berhampore, Haldia) with ZIP
2. Plan-wise filtering (Alacarte vs Bouquet)
3. LCO pivot table for wallet consumption
4. LCO-wise export (individual CSVs + ZIP)
5. Credit/Debit note extraction
6. Direct customer billing (RTU)
7. Broadcaster-wise wallet revenue
8. Revenue share calculation (broadcaster deduction %)
9. Plan-wise vs Service-wise amount breakdown
10. Month-wise transaction grouping
11. Hourly transaction analysis

**Inputs:** Wallet report, credit/debit notes, service details with broadcaster, plan type config
**Outputs:**
- `Berhampore_Haldia_*.zip` (area-wise)
- `LCOWise_Wallet_Report_*.zip` (LCO-wise)
- Multiple monthly files: `Meghbela_Subs_Bill_YYYY-MM.csv`
- `LCOWISE_WALLET_SUMMARY_*.csv`
- `RTU_customers_bill_amount_*.csv`
- Service-wise and plan-wise breakdowns

**Dependencies:** tidyverse, dplyr, readxl, stringr, lubridate

**Key Code:**
```r
# Area filter
filter(str_detect(Entity.Code, "MSW"))  # Berhampore

# LCO pivot
group_by(Entity.Code) %>%
  summarize(Total_debit=sum(Amount.Debit))

# ZIP creation
zip(zipfile=sprintf("Output/LCOWise_Wallet_Report_%s_%g",
                    month(...), year(...)))

# Broadcaster deduction
mutate(Broadcaster.part_WO_TAX = ((Amount.Debit/1.18)*0.889))

# Month-wise grouping
wallet_filt$Month.Year <- format(wallet_filt$Transaction.Date, "%Y-%m")
for (my in unique(wallet_filt$Month.Year)) {
  month_data <- wallet_filt[wallet_filt$Month.Year==my, ]
  write.csv(...)
}
```

---

#### Wallet_reconcile.R
**Size:** ~5 KB
**Purpose:** Complete wallet reconciliation with variance analysis

**Key Functions:**
1. Reads opening balance, closing balance, payment details, credit/debit notes, wallet consumption
2. Filters operational entity payments
3. Groups payments by LCO code
4. Splits credit/debit notes by type (CR/DR)
5. Merges all components
6. Calculates closing: Opening + Payments + Credits - Debits - Consumption
7. Compares calculated vs MQ reported closing
8. Calculates variance for investigation
9. Identifies promotional vs paid customers
10. Extracts filename for dynamic output naming

**Inputs:** Opening balance, closing balance, payment details, credit/debit notes, wallet summary
**Outputs:**
- `*_WALLET_RECONCILE.CSV` (variance column)
- `AllCustomers_*.csv` (paid vs promo)

**Dependencies:** tidyverse, dplyr, readxl, purrr

**Key Code:**
```r
# Payment summary
filter(Party.Type=="OPERATIONAL ENTITY") %>%
  group_by(Entity.Code) %>%
  summarise(Total.Payments=sum(Amount))

# Closing calculation
mutate(Calculated.Closing.Balance =
       ifelse(Balance.Type=='CR', Opening.Balance, -Opening.Balance) +
       Total.Payments + Credit.Note - Debit.Note - Wallet.Consumption)

# Variance
mutate(Difference = Closing.MQ.Report - Calculated.Closing.Balance)
```

---

#### Wallet_Revenue_Paid_Free_Analysis.R
**Purpose:** Revenue breakdown by payment type
**Status:** Active monthly analysis

---

### TOOLS SCRIPTS (Source/Tools/)

#### Audit_MSR_calc.R
**Purpose:** MSR calculation audit across all CAS systems
**Status:** Validation and quality assurance

---

#### CAS_System_namemaker.R
**Purpose:** CAS system naming conventions
**Status:** Utility for consistent naming

---

#### create_package.R
**Purpose:** Package creation and configuration utility
**Status:** Package management

---

#### cust_historical.R
**Purpose:** Historical customer journey tracking
**Status:** Historical analysis

---

#### Customer_Details.R
**Purpose:** Extract detailed customer information
**Status:** Data extraction utility

---

#### DailyWork.R
**Size:** ~4 KB
**Purpose:** Track daily work activities of SMS users

**Key Functions:**
1. Reads SMS system user activity logs
2. Filters for specific users (CS team)
3. Parses transaction timestamps
4. Filters transactions after 10 AM
5. Removes provisioning logs
6. Removes incomplete transactions
7. Filters for target transaction types (Add Contract, Disconnect, Modify)
8. Creates pivot (users vs transaction types)
9. Adds row and column totals
10. Exports detailed and summary reports

**Inputs:** SMS system user activity log
**Outputs:**
- `Output/SMS_Work_Count.csv` (pivot with totals)
- `Output/SMS_Work_Details.csv` (detailed transactions)

**Dependencies:** tidyverse, dplyr, readxl, stringr, knitr, janitor, lubridate

**Key Code:**
```r
# Time filter
mutate(since_midnight=hour(Transaction.Date)*60 + minute(Transaction.Date)) %>%
  filter(since_midnight>=10)

# Pivot with totals
group_by(User, Transaction.Type) %>%
  summarize(Transaction.Count=n()) %>%
  pivot_wider(names_from=Transaction.Type, values_from=Transaction.Count) %>%
  adorn_totals(where=c("row","col"))
```

---

#### Date_filter_areawise.R
**Purpose:** Filter data by date with area-wise grouping
**Status:** Data filtering utility

---

#### Disconnected_aging.R
**Size:** ~3 KB
**Purpose:** Analyze disconnected customer aging and renewal behavior

**Key Functions:**
1. Reads daily disconnected customers list
2. Reads base plan wallet transactions
3. Merges disconnection with most recent wallet transaction
4. Calculates days between disconnection and last transaction
5. Groups by aging days (percentage distribution)
6. Separates customers who never recharged (NA date_diff)
7. Exports non-renewed customers

**Inputs:** Disconnection list, wallet report, plan names (Google)
**Outputs:** `Output/NotRenewed.csv`
**Dependencies:** tidyverse, dplyr

**Key Code:**
```r
# Date difference
date_diff <- as.Date(as.POSIXct(Transaction.Date, format="%d/%m/%Y %I:%M:%S %p")) -
             as.Date(as.POSIXct(Disconnection.Date, format="%d/%m/%Y %I:%M:%S %p"))

# Aging pivot
group_by(date_diff) %>%
  summarize(count_=n()) %>%
  mutate(percentage=count_/sum(count_)*100)
```

---

#### DPOpacktoBroadcaster.R
**Purpose:** Map DPO packages to broadcasters
**Status:** Package mapping utility

---

#### Gospell_command_eval.R
**Size:** ~6 KB
**Purpose:** Reconcile Gospell CAS with MQ system

**Key Functions:**
1. Reads MQ active list and inventory
2. Reads Gospell CAS data (VC+CASCODE pairs)
3. Combines VC+CASCODE for unique ID
4. Filters for Gospell customers only
5. Merges MQ with Gospell CAS
6. Identifies Gospell services not in MQ (inactive)
7. Reads Gospell disconnection log (historical)
8. Identifies cards with future disconnect dates (permanent)
9. Combines both sources for inactive list
10. Joins with inventory for area assignment
11. Exports inactive commands per area

**Inputs:** MQ active list, inventory, Gospell CAS data, Gospell disconnect log (optional)
**Outputs:**
- `Output/GOSPELL INACTIVE COMMANDS RECEND.CSV`
- Area-wise CSV exports

**Dependencies:** tidyverse, here, dplyr, readxl

**Key Code:**
```r
# VC+CASCODE combine
unite(combined, c("VC","CASCODE"))

# Inactive filter
merge(GSPL_cas_data, mq_GSPL_data, by="combined", all.x=T) %>%
  filter(is.na(CUSTOMER_NBR))

# Area export loop
for (ENTITY_CODE in unique_lco_codes) {
  area_data <- filter(GSPL_all, ENTITY_CODE.y==ENTITY_CODE)
  write.csv(...)
}
```

---

#### historical.R
**Size:** ~3 KB
**Purpose:** Analyze customer and LCO master with STB distribution

**Key Functions:**
1. Reads total customers network file
2. Groups by STB type and counts customers
3. Sorts by customer count (descending)
4. Reads LCO master file
5. Cleans LCO codes (removes quotes)
6. Removes blank city entries
7. Exports city-wise LCO files
8. Merges LCO master with location data

**Inputs:** Customer network file, LCO master
**Outputs:**
- `STBwise Customer.CSV`
- `LcoMaster.csv`
- City-wise CSV files

**Dependencies:** tidyverse, here, dplyr

**Key Code:**
```r
# STB count
group_by(STB.Item.Descr) %>%
  summarize(CustomerCount=n()) %>%
  arrange(desc(CustomerCount))

# City loop
for (city in city.list) {
  lco_master_filt = filter(lco_master, City==city)
  write.csv(...)
}
```

---

#### LcoPaymentCalc.R
**Size:** ~4 KB
**Purpose:** Calculate LCO payment requirements with pro-rata

**Key Functions:**
1. Specifies LCO code and calculation date
2. Reads MQ active list and due for renewal dates
3. Reads LCO-specific pricing configuration
4. Filters active customers for LCO
5. Filters customers renewing by date
6. Merges pricing with customer plan data
7. Calculates pro-rata: (Price/30)*Days*1.18 (with tax)
8. Exports payment requirement file

**Inputs:** MQ active list, due for renewal list, LCO pricing config, renewal requirements
**Outputs:** `LCO_Price_required_byDate.csv`
**Dependencies:** tidyverse, dplyr, lubridate

**Key Code:**
```r
# LCO filter
filter(ENTITY_CODE %in% lcoCode)

# Pro-rata calculation
mutate(FullAmnt=(Price/30)*Days) %>%
  mutate(FinalAmnt=FullAmnt*1.18)

# Alacarte plan code swap
Plan.Code[Plan.Category=='Alacarte'] <- Service.Code[Plan.Category=='Alacarte']
```

---

#### LcoPaymentTrends.R
**Purpose:** LCO payment trend analysis
**Status:** Financial trend tracking

---

#### lcowise_stb_stat.R
**Purpose:** LCO-wise STB statistics
**Status:** Inventory reporting

---

#### MERGE_CREATE.R
**Purpose:** Merge multiple data sources
**Status:** Data consolidation utility

---

#### MSR_details.R
**Purpose:** Extract MSR-level detail records
**Status:** MSR detail extraction

---

#### OnlinePaymentRecon.R
**Purpose:** Reconcile online payments (HDFC) with wallet
**Status:** Payment reconciliation

---

#### PackageCosting.R
**Purpose:** Package cost calculations
**Status:** Pricing utility

---

#### pmrreportobsolete.R
**Purpose:** PMR report (deprecated)
**Status:** Obsolete - use PMR_REPORT_MAKE_NEW.R instead

---

#### Star_Value_calc.R
**Purpose:** Calculate Star India broadcaster value assignments
**Status:** Broadcaster-specific calculations

---

#### stock_report.R
**Purpose:** STB/SC inventory stock reporting
**Status:** Inventory management

---

#### Subscriber_Trend_analysis.R
**Size:** ~8 KB
**Purpose:** Visualize subscriber trends and patterns

**Key Functions:**
1. Loads historical monthly customer data
2. Cleans and normalizes service names
3. Replaces aliases (DD → Bronze Basic)
4. Creates subscriber summary by city/LCO/plan
5. Generates visualizations (ggplot2):
   - Migration patterns
   - Contract change patterns
   - Package type trends
   - Weight distribution
6. Exports PNG plots to plots/ directory

**Inputs:** Historical customer data (monthly)
**Outputs:** PNG files in plots/ directory
**Dependencies:** tidyverse, dplyr, ggplot2, lubridate

**Key Code:**
```r
# Service name normalization
mutate(Service = gsub("DD Free Dish", "Bronze Basic", Service))

# Trend visualization
ggplot(data, aes(x=Month, y=Subscriber_Count, color=Plan_Type)) +
  geom_line() +
  geom_point()

# Export
ggsave("plots/migration_patterns.png")
```

---

#### Wallet_ListActive_Recon.R
**Purpose:** Reconcile wallet transactions with active list
**Status:** Wallet-active reconciliation

---

## Code Snippet Library

### 1. DATA LOADING

#### Import MQ Active List (Standardized)
**Location:** `Source/Functions.R`
```r
mq_active_report <- function() {
  library(tidyverse)
  library(dplyr)

  # File picker
  list_active <- read.csv(file.choose())

  # Standardize column names
  names(list_active) <- gsub("\\.", "_", toupper(names(list_active)))

  # Remove quotes from smart card/STB fields
  list_active$SMART_CARD_NBR <- gsub("'", "", list_active$SMART_CARD_NBR)
  list_active$STB_SERIAL_NBR <- gsub("'", "", list_active$STB_SERIAL_NBR)

  # Add CAS system identification by card length
  list_active$VC.length <- nchar(as.character(list_active$SMART_CARD_NBR))
  list_active$VC.length <- gsub("8","GOSPELL",
                          gsub("12","SAFEVIEW",
                          gsub("15","NAGRA",
                          gsub("16","ABV",list_active$VC.length))))

  return(list_active)
}
```
**Used in:** 15+ scripts

---

#### Load Google Sheets Data
**Location:** Multiple scripts (e.g., `Source/Monthly/active_analysis.R`)
```r
# Direct CSV download from Google Sheets
plan_names <- read.csv("https://docs.google.com/spreadsheets/d/.../export?format=csv")

# Or using Google Sheets feed
library(readr)
url <- "https://docs.google.com/spreadsheets/d/.../pub?output=csv"
data <- read_csv(url)
```
**Used in:** 10+ scripts

---

#### Load Excel Multi-Sheet
**Location:** Multiple scripts (e.g., `Source/Monthly/AreawiseMSR.R`)
```r
library(readxl)

# Get all sheet names
sheets <- excel_sheets("file.xlsx")

# Filter for specific sheets
data_sheets <- sheets[grepl("CumulativeActiveSMCReport", sheets)]

# Read specific sheet with skip rows
data <- read_excel("file.xlsx", sheet="Sheet1", skip=3)

# Read all sheets into list
all_data <- lapply(sheets, function(x) read_excel("file.xlsx", sheet=x))
```
**Used in:** 8+ scripts

---

### 2. CAS SYSTEM OPERATIONS

#### Identify CAS System by Card Length
**Location:** `Source/Functions.R`
```r
# Add CAS system column based on smart card length
df$VC.length <- nchar(as.character(df$SMART_CARD_NBR))
df$VC.length <- gsub("8","GOSPELL",
               gsub("12","SAFEVIEW",
               gsub("15","NAGRA",
               gsub("16","ABV",df$VC.length))))

# Map: 8 chars = Gospell
#      12 chars = Safeview
#      15 chars = Nagra
#      16 chars = ABV
```
**Used in:** 12+ scripts

---

#### Filter Inventory (STB Only, No Smart Cards)
**Location:** `Source/new_lco_active.R`
```r
inventory_stb <- inventory %>%
  filter(!(str_detect(ITEM_CODE, "SC"))) %>%
  filter(!(str_detect(ITEM_CODE, "NAGRA")))
```
**Used in:** 5+ scripts

---

### 3. DATA TRANSFORMATION

#### Group and Count by Entity/LCO
**Location:** Nearly all scripts
```r
# Simple count
lco_summary <- df %>%
  group_by(ENTITY_CODE) %>%
  summarise(Total_Count = n())

# With multiple aggregations
lco_summary <- df %>%
  group_by(ENTITY_CODE, ENTITY_NAME) %>%
  summarise(
    Total_Active = n(),
    Total_Revenue = sum(Amount.Debit, na.rm=TRUE),
    Avg_Amount = mean(Amount.Debit, na.rm=TRUE)
  )
```
**Used in:** 40+ scripts

---

#### Pivot Wide (Create Matrix)
**Location:** Multiple scripts (e.g., `Source/Monthly/active_analysis.R`)
```r
# Broadcaster pivot
broadcaster_pivot <- df %>%
  group_by(CUSTOMER_NBR, Broadcaster) %>%
  summarize(ServiceCount = n()) %>%
  pivot_wider(names_from = Broadcaster,
              values_from = ServiceCount,
              values_fill = 0)

# Weekly progression pivot
weekly_pivot <- df %>%
  group_by(Service, Week) %>%
  summarize(Count = n()) %>%
  pivot_wider(names_from = Week,
              values_from = Count,
              values_fill = 0)
```
**Used in:** 20+ scripts

---

#### Merge 4-Week Snapshots
**Location:** `Source/Monthly/BC_Report_ZEE.R`, `Source/Monthly/AreawiseMSR.R`
```r
# Merge all 4 weeks
msr_combined <- merge(msr_7th, msr_14th, all=T) %>%
                merge(msr_21st, all=T) %>%
                merge(msr_28th, all=T)

# Calculate monthly average
msr_combined <- msr_combined %>%
  mutate(Monthly_Avg = rowMeans(select(., starts_with("Active_")), na.rm=TRUE))
```
**Used in:** 6+ scripts

---

#### Replace NA with 0
**Location:** Multiple scripts
```r
# Single column
df$Column[is.na(df$Column)] <- 0

# All columns
df <- df %>%
  mutate(across(everything(), ~ replace_na(., 0)))

# Specific columns
df <- df %>%
  mutate(across(c(Col1, Col2, Col3), ~ replace_na(., 0)))
```
**Used in:** 15+ scripts

---

### 4. RECONCILIATION PATTERNS

#### MSR vs List Active Reconciliation
**Location:** `Source/Monthly/active_analysis.R`, `Source/Monthly/BC_Report.R`
```r
# Find in MSR but not in List Active
missing_in_active <- merge(msr_data, active_data,
                           by.x="Service.Code", by.y="SERVICE_CODE",
                           all.x=T) %>%
                     filter(is.na(ENTITY_CODE))

# Find in List Active but not in MSR
missing_in_msr <- merge(active_data, msr_data,
                        by.x="SERVICE_CODE", by.y="Service.Code",
                        all.x=T) %>%
                  filter(is.na(Broadcaster))
```
**Used in:** 5+ scripts

---

#### CAS vs MQ Reconciliation
**Location:** `Source/Tools/Gospell_command_eval.R`, `Source/Monthly/Reconcilliation.R`
```r
# Left join CAS with MQ, filter for NA = inactive in MQ
inactive_cas <- merge(cas_data, mq_data,
                      by="smart_card",
                      all.x=T) %>%
                filter(is.na(CUSTOMER_NBR))
```
**Used in:** 3+ scripts

---

#### Wallet vs Active Validation
**Location:** `Source/Monthly/ActiveCustomer.R`
```r
# Count unique wallet customers
wallet_count <- wallet %>%
  select(Customer.Nbr) %>%
  unique() %>%
  group_by(1) %>%
  summarise(WallCount = n())

# Merge and take maximum
combined <- merge(active_count, wallet_count, all.x=T) %>%
            mutate(across(everything(), ~ replace_na(., 0))) %>%
            mutate(Final_Active = pmax(Active_Count, WallCount))
```
**Used in:** 2 scripts

---

### 5. CALCULATIONS

#### Pro-rata Calculation (with Tax)
**Location:** `Source/refund_find.R`, `Source/Tools/LcoPaymentCalc.R`
```r
# Pro-rata refund
credit_amount <- (Amount.Debit / Billing.Frequency) *
                 as.numeric((Billing.Frequency - (End.Date - Start.Date)))

# Pro-rata payment with tax
payment_amount <- (Price / 30) * Days
payment_with_tax <- payment_amount * 1.18  # Add 18% GST
```
**Used in:** 5+ scripts

---

#### Revenue Share Calculation (Broadcaster Deduction)
**Location:** `Source/Monthly/Wallet_analysis.R`, `Source/Monthly/Broadcaster_wallet.R`
```r
# For LCO customers (88.9% to broadcaster)
broadcaster_share <- (Amount.Debit / 1.18) * 0.889  # Remove tax, then 88.9%

# For direct customers (80% to broadcaster)
broadcaster_share <- (Amount.Debit / 1.18) * 0.80

# Tax removal
amount_without_tax <- Amount.Debit / 1.18
```
**Used in:** 3 scripts

---

#### Penetration Percentage
**Location:** Multiple scripts (e.g., `Source/Monthly/BC_Report.R`)
```r
# Calculate penetration
penetration <- paste0(round((Active_Count / Total_Customers) * 100, 2), "%")

# With mutate
df <- df %>%
  mutate(Penetration = paste0(round(Active/Total*100, 2), "%"))
```
**Used in:** 8+ scripts

---

#### Capacity Utilization (Active/STB Ratio)
**Location:** `Source/new_lco_active.R`
```r
capacity <- df %>%
  mutate(Percent_Active = paste0(round(Total_Active/Total_STB*100, 2), "%"))
```
**Used in:** 2 scripts

---

### 6. DATE/TIME OPERATIONS

#### Parse Date Formats
**Location:** Multiple scripts
```r
library(lubridate)

# Standard format
date <- as.Date(date_string, format="%d/%m/%Y")

# Timestamp format
date <- as.Date(as.POSIXct(date_string, format="%d/%m/%Y %I:%M:%S %p"))

# Using lubridate
date <- dmy(date_string)  # Day-Month-Year
date <- mdy(date_string)  # Month-Day-Year
date <- ymd(date_string)  # Year-Month-Day

# Parse multiple formats
date <- parse_date_time(date_string, orders=c("dmy", "mdy", "ymd"))
```
**Used in:** 20+ scripts

---

#### Date Arithmetic
**Location:** Multiple scripts
```r
# Add/subtract days
tomorrow <- today() + 1
yesterday <- today() - 1
next_month <- today() + months(1)

# Date difference
days_diff <- as.numeric(end_date - start_date)

# Filter by date range
df %>% filter(Contract.End.Date == today())
df %>% filter(Contract.End.Date >= today() & Contract.End.Date <= today()+30)
```
**Used in:** 15+ scripts

---

#### Extract Month/Year
**Location:** `Source/Monthly/Wallet_analysis.R`
```r
# Month-year string
df$Month_Year <- format(df$Transaction.Date, "%Y-%m")

# Month name
month_name <- month(today()-months(1), label=TRUE, abbr=FALSE)

# Dynamic filename with month/year
filename <- sprintf("Report_%s_%g.csv", month(today()), year(today()))
```
**Used in:** 10+ scripts

---

### 7. FILE EXPORT OPERATIONS

#### Dated Filename Pattern
**Location:** 15+ scripts
```r
# Date in filename (DDMMYYYY)
filename <- sprintf("Report_%s.csv", format(Sys.Date(), "%d%m%Y"))

# Month and year
filename <- sprintf("Report_%s_%g.csv", month(today()), year(today()))

# Full date components
filename <- paste0("Report_", day(today()), month(today()), year(today()), ".csv")
```
**Used in:** 15+ scripts

---

#### Multi-Sheet Excel Export
**Location:** `Source/Monthly/Penetration_Percentage.R`
```r
library(openxlsx)

# Create list of dataframes
excel_list <- list(
  "Sheet1" = df1,
  "Sheet2" = df2,
  "Sheet3" = df3
)

# Write to Excel
write.xlsx(excel_list, "output.xlsx")

# Alternative with xlsx package
library(xlsx)
write.xlsx(df1, "output.xlsx", sheetName="Sheet1", row.names=FALSE)
write.xlsx(df2, "output.xlsx", sheetName="Sheet2", append=TRUE, row.names=FALSE)
```
**Used in:** 7+ scripts

---

#### ZIP Compression
**Location:** `Source/Monthly/Wallet_analysis.R`
```r
# Create ZIP of multiple files
files_to_zip <- c("file1.csv", "file2.csv", "file3.csv")
zip(zipfile = "Output/Archive.zip", files = files_to_zip)

# Dynamic ZIP name
zipname <- sprintf("Output/LCOWise_Report_%s_%g.zip", month(today()), year(today()))
zip(zipfile = zipname, files = file_list)
```
**Used in:** 2 scripts

---

#### Loop Export (LCO-wise Files)
**Location:** `Source/Monthly/Wallet_analysis.R`, `Source/Tools/historical.R`
```r
# Export one file per LCO
for (lco_code in unique(df$Entity.Code)) {
  lco_data <- df %>% filter(Entity.Code == lco_code)
  filename <- paste0("Output/LCO_", lco_code, ".csv")
  write.csv(lco_data, filename, row.names=FALSE)
}

# With progress tracking
for (i in seq_along(unique_codes)) {
  cat(i, "/", length(unique_codes), "\n")
  # ... export code ...
}
```
**Used in:** 5+ scripts

---

### 8. API OPERATIONS

#### MQ API - XML Request (RETRACK)
**Location:** `Source/retrack_startToday.R`
```r
library(httr)

# XML body construction
body_xml <- paste0("<RETRACK>\r\n",
                   "<CUSTOMERNUMBER>", customer_number, "</CUSTOMERNUMBER>\r\n",
                   "<HARDWAREREFNUMBER>", smart_card, "</HARDWAREREFNUMBER>\r\n",
                   "<STARTDATE>", start_date, "</STARTDATE>\r\n",
                   "</RETRACK>")

# Headers
headers <- c(
  'USERNAME' = 'MB102',
  'PASSWORD' = 'your_password',
  'EXTERNALPARTY' = 'MQS'
)

# POST request
response <- VERB("POST",
                 url = "http://your-api-url/endpoint",
                 body = body_xml,
                 add_headers(headers),
                 content_type("application/xml"))

# Check response
print(content(response, "text"))
```
**Used in:** 5+ scripts (retrack variants, disconnect/reconnect)

---

#### SMS API Integration
**Location:** `Source/Functions.R`
```r
library(httr)

# SMS API call
send_sms <- function(mobile, message) {
  url <- "https://rapidsms.co.in/api/send_sms"

  response <- GET(url,
                  query = list(
                    username = "your_username",
                    password = "your_password",
                    mobile = mobile,
                    message = message
                  ))

  return(content(response, "text"))
}
```
**Used in:** 2 scripts

---

#### Reference Number Generation
**Location:** `Source/resendBose_startTomorrow.R`
```r
# Generate unique reference number
ref_num <- paste0(format(Sys.time(), "%d%m%Y%H%M%S"), "ABRTEY")

# Alternative with random suffix
ref_num <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"),
                  sample(1000:9999, 1))
```
**Used in:** 3+ scripts

---

### 9. DATA CLEANING

#### Plan Name Standardization
**Location:** `Source/Monthly/BC_Report.R`, `Source/testbench.R`
```r
# Remove promotional suffixes
plan_clean <- gsub("\\(Promo.*\\)", "", plan_name)
plan_clean <- gsub("\\(PROMO.*\\)", "", plan_clean)

# Standardize plan codes
df$Package <- recode(df$Package,
                     'CLASSIC HINDI @ 300' = 'CLASSIC_HIN_300',
                     'CLASSIC HINDI @ 325' = 'CLASSIC_HIN_325',
                     'BASIC PLAN @ 130' = 'BASIC_130')

# Multiple gsub chain
plan_clean <- gsub(" \\(Promo\\)", "", plan_name) %>%
              gsub(" - OLD", "", .) %>%
              gsub("  ", " ", .)
```
**Used in:** 10+ scripts

---

#### Remove Quotes and Special Characters
**Location:** `Source/Functions.R`, multiple scripts
```r
# Remove single quotes
df$Column <- gsub("'", "", df$Column)

# Remove all quotes
df$Column <- gsub("[\"']", "", df$Column)

# Clean LCO codes
df$Lco.Code <- gsub("'", "", df$Lco.Code)
```
**Used in:** 15+ scripts

---

#### Filter Out Blanks/NAs
**Location:** Multiple scripts
```r
# Remove NA rows
df <- df %>% filter(!is.na(Important_Column))

# Remove blanks
df <- df %>% filter(!(Column == "" | is.na(Column)))

# Remove both
df <- df %>% filter(!(is.na(Column) | Column == ""))

# Clean city data
df <- df %>% filter(!(is.na(City) | City == ""))
```
**Used in:** 20+ scripts

---

### 10. SPECIAL OPERATIONS

#### Greedy Assignment Algorithm
**Location:** `Source/Monthly/FInd_nopair_box.R`
```r
# Initialize tracking column
blank_customers$used <- FALSE

# Greedy loop
for (i in seq_len(nrow(discontinued_df))) {
  # Find matching unused customers
  match_idx <- which(!blank_customers$used &
                     blank_customers$ENTITY_CODE == discontinued_df$ENTITY_CODE[i])

  if (length(match_idx) > 0) {
    # Choose first match
    chosen_idx <- match_idx[1]

    # Assign
    discontinued_df$assigned_customer[i] <- blank_customers$Customer.Number[chosen_idx]

    # Mark as used
    blank_customers$used[chosen_idx] <- TRUE
  }
}
```
**Used in:** 1 script (unique algorithm)

---

#### Combine Multiple Columns into One
**Location:** Multiple scripts
```r
library(tidyr)

# Unite columns
df <- df %>%
  unite(combined, c("Column1", "Column2"), sep="_", remove=FALSE)

# Example: VC + CASCODE
df <- df %>%
  unite(unique_id, c("VC", "CASCODE"), sep="")
```
**Used in:** 5+ scripts

---

#### Split Column into Multiple
**Location:** `Source/Monthly/CAS_data_make.R`
```r
# Separate by position
df <- df %>%
  separate(SERIAL_NUMBER,
           into = c("leftval", "rightval"),
           sep = 10)  # Split at position 10

# Separate by delimiter
df <- df %>%
  separate(Full_Name,
           into = c("First_Name", "Last_Name"),
           sep = " ")
```
**Used in:** 3+ scripts

---

#### Dynamic Column Selection
**Location:** Multiple scripts
```r
# Select columns starting with prefix
df %>% select(starts_with("Active_"))

# Select columns ending with suffix
df %>% select(ends_with("_Count"))

# Select columns containing pattern
df %>% select(contains("Revenue"))

# Combine in mutate
df %>% mutate(Average = rowMeans(select(., starts_with("Active_")), na.rm=TRUE))
```
**Used in:** 10+ scripts

---

## Duplication Map

### HIGH DUPLICATION (Consolidation Recommended)

#### 1. **4-Week MSR Processing Pattern**
**Found in:**
- `Source/Monthly/BC_Report_ZEE.R` ⭐ **BEST**
- `Source/Monthly/BC_report_areawise.R`
- `Source/Monthly/AreawiseMSR.R`
- `Source/Monthly/ListActivetoMSR.R`

**Similarity:** 90%
**Pattern:** Merge 7th, 14th, 21st, 28th day snapshots → Calculate monthly average
**Recommendation:** Extract to shared function in `Functions.R`

**Best Implementation:** `BC_Report_ZEE.R` - cleanest separation, well-commented

---

#### 2. **CAS System Identification by Card Length**
**Found in:**
- `Source/Functions.R` ⭐ **SOURCE OF TRUTH**
- `Source/testbench.R`
- `Source/new_lco_active.R`
- 10+ other scripts (repeated inline)

**Similarity:** 100%
**Pattern:** `nchar()` → `gsub()` chain for 8/12/15/16 character mapping
**Recommendation:** **Already centralized in Functions.R** - all scripts should call it from there

**Action Needed:** Remove inline duplicates, use `Functions.R::mq_active_report()`

---

#### 3. **Active Customer Grouping and Counting**
**Found in:**
- `Source/Monthly/BC_Report.R` ⭐ **MOST COMPREHENSIVE**
- `Source/Monthly/active_analysis.R`
- `Source/Monthly/Penetration_Percentage.R`
- `Source/Monthly/ActiveCustomer.R`

**Similarity:** 75%
**Pattern:** Group by entity/plan/broadcaster → Count → Pivot
**Recommendation:** Create parameterized function: `group_and_count(df, group_by_cols, count_col)`

**Best Implementation:** `BC_Report.R` - includes broadcaster mapping and penetration

---

#### 4. **Wallet Transaction Processing**
**Found in:**
- `Source/Monthly/Wallet_reconcile.R` ⭐ **MOST COMPLETE**
- `Source/Monthly/Wallet_analysis.R`
- `Source/Monthly/Broadcaster_wallet.R`

**Similarity:** 60%
**Pattern:** Load wallet → Filter transactions → Group by various dimensions
**Recommendation:** Keep separate (different purposes) but extract common preprocessing to function

**Best Implementation:** `Wallet_reconcile.R` - comprehensive balance calc with variance

---

#### 5. **API Operations (RETRACK/Disconnect)**
**Found in:**
- `Source/resendBose_startTomorrow.R` ⭐ **MOST COMPLETE** (dual operations)
- `Source/retrack_startToday.R`
- `Source/retrack_startTomorrow.R`
- `Source/retrack_endToday.R`

**Similarity:** 85%
**Pattern:** Load list → Filter by date → Create XML → POST to API
**Recommendation:** Consolidate into single parameterized script with date parameter

**Best Implementation:** `resendBose_startTomorrow.R` - handles both disconnect and reconnect

---

### MODERATE DUPLICATION (Extract Common Functions)

#### 6. **LCO-wise Export Loop**
**Found in:**
- `Source/Monthly/Wallet_analysis.R`
- `Source/Tools/historical.R`
- `Source/Tools/Gospell_command_eval.R`

**Similarity:** 70%
**Pattern:** `for (code in unique(df$LCO_Code)) { filter → write.csv }`
**Recommendation:** Create utility function: `export_by_lco(df, lco_col, output_dir)`

---

#### 7. **Date Filtering for Renewals**
**Found in:**
- All retrack scripts (3 variants)
- `Source/Tools/LcoPaymentCalc.R`

**Similarity:** 80%
**Pattern:** Filter for `Contract.End.Date == today()` or `today()+X`
**Recommendation:** Create utility: `filter_by_renewal_date(df, date_offset)`

---

#### 8. **Excel Multi-Sheet Reading**
**Found in:**
- `Source/Monthly/CAS_data_make.R`
- `Source/Monthly/AreawiseMSR.R`
- 5+ other scripts

**Similarity:** 65%
**Pattern:** `excel_sheets()` → filter → loop read
**Recommendation:** Create utility: `read_excel_filtered(file, sheet_pattern, skip_rows)`

---

### LOW DUPLICATION (Keep Separate)

#### 9. **Unique or Specialized Functions**
**No Duplication:**
- `Source/Forecasting.R` - Prophet/Holt's forecasting (unique)
- `Source/shiny_test.R` - Interactive UI (unique)
- `Source/Tools/DailyWork.R` - SMS user tracking (unique)
- `Source/Monthly/IPTVMSR_v2.R` - IPTV state-wise (specialized)
- `Source/Monthly/FInd_nopair_box.R` - Greedy assignment (unique algorithm)

**Recommendation:** Keep as-is, no consolidation needed

---

## Duplication Summary Table

| Pattern | Scripts Affected | Duplication % | Best Script | Recommendation |
|---------|------------------|---------------|-------------|----------------|
| 4-week MSR merge | 4 scripts | 90% | BC_Report_ZEE.R | Extract to Functions.R |
| CAS system ID | 12+ scripts | 100% | Functions.R | **Already exists** - remove inline copies |
| Active grouping | 4 scripts | 75% | BC_Report.R | Create parameterized function |
| Wallet processing | 3 scripts | 60% | Wallet_reconcile.R | Extract preprocessing only |
| API operations | 4 scripts | 85% | resendBose_startTomorrow.R | Consolidate with parameters |
| LCO export loop | 3 scripts | 70% | - | Create utility function |
| Date filtering | 5+ scripts | 80% | - | Create utility function |
| Excel multi-sheet | 6+ scripts | 65% | - | Create utility function |

---

## Input/Output Matrix

### INPUT FILES

| File Pattern | Read By (Scripts) | Purpose |
|--------------|-------------------|---------|
| MQ Active Customer List | 15+ scripts | Primary data source for all analyses |
| Wallet Report | `Wallet_analysis.R`, `Wallet_reconcile.R`, `ActiveCustomer.R`, `Broadcaster_wallet.R`, `Disconnected_aging.R`, `refund_find.R` | Revenue and transaction analysis |
| Inventory File | `new_lco_active.R`, `Gospell_command_eval.R`, `CAS_data_make.R`, `FInd_nopair_box.R` | Hardware tracking (STB/VC) |
| Package Details (Google Sheets) | `active_analysis.R`, `BC_Report.R`, `Broadcaster_wallet.R`, `Penetration_Percentage.R` | Service-broadcaster mapping |
| MSR Reports (Excel) | `AreawiseMSR.R`, `BC_report_areawise.R`, `BC_Report_ZEE.R`, `active_analysis.R` | Broadcaster reconciliation |
| Due for Renewal List | All retrack scripts, `resendBose_startTomorrow.R`, `LcoPaymentCalc.R` | Contract expiry operations |
| CAS System Data (Gospell/Safeview/ABV/Nagra) | `Reconcilliation.R`, `Gospell_command_eval.R`, `CAS_data_make.R` | CAS reconciliation |
| LCO Master | `new_lco_active.R`, Multiple scripts | Entity mapping |
| Credit/Debit Notes | `Wallet_reconcile.R`, `Wallet_analysis.R`, `refund_find.R` | Financial adjustments |
| IPTV Subscription Files | `IPTVMSR.R`, `IPTVMSR_v2.R` | IPTV reporting |

---

### OUTPUT FILES

| File Pattern | Created By (Scripts) | Used By (Scripts) | Purpose |
|--------------|---------------------|-------------------|---------|
| `MSR_Report_all_*.xlsx` | `ListActivetoMSR.R` | Manual reporting | Broadcaster settlements |
| `*_IPTV_MSR_all_*.xlsx` | `IPTVMSR_v2.R` | Manual reporting | State-wise IPTV reports |
| `LCOWISE_WALLET_SUMMARY_*.csv` | `Wallet_analysis.R` | `Wallet_reconcile.R` | LCO revenue summaries |
| `listactive_to_MSR*.CSV` | `active_analysis.R`, `BC_Report.R` | Manual analysis | MSR reconciliation |
| `LCO_Price_required_byDate.csv` | `LcoPaymentCalc.R` | Manual payment processing | LCO payment requirements |
| `New_LCO_data_*.CSV` | `new_lco_active.R` | Manual analysis | Active vs STB reconciliation |
| `credit_fcn.csv` | `refund_find.R` | Manual processing | Refund calculations |
| `*_WALLET_RECONCILE.CSV` | `Wallet_reconcile.R` | Manual review | Balance variance analysis |
| `Blank_box_with_blank_customer_number.csv` | `FInd_nopair_box.R` | Manual assignment | Discontinued hardware tracking |
| PNG plots (plots/) | `Subscriber_Trend_analysis.R` | Reports/presentations | Trend visualizations |
| `Output/SMS_Work_Count.csv` | `DailyWork.R` | Management review | User performance tracking |
| ZIP archives | `Wallet_analysis.R` | Distribution to LCOs | LCO-wise reports |

---

## Workflow Dependencies

### Monthly Reporting Chain
```
1. MQ Active List Export (external)
   ↓
2. ListActivetoMSR.R → MSR_Report_all_*.xlsx
   ↓
3. active_analysis.R → Reconciliation files
   ↓
4. BC_Report.R → Broadcaster reports
   ↓
5. Wallet_analysis.R → LCO revenue summaries
   ↓
6. Wallet_reconcile.R → Balance variance
```

### IPTV Reporting Chain
```
1. IPTV Subscription Export (external)
   ↓
2. IPTVMSR_v2.R → State-wise MSR reports
```

### CAS Reconciliation Chain
```
1. CAS System Exports (Gospell/Safeview/ABV/Nagra)
   ↓
2. CAS_data_make.R → Clean CAS data
   ↓
3. Reconcilliation.R or Gospell_command_eval.R → Mismatch reports
```

### Payment Processing Chain
```
1. Wallet Report + Due for Renewal
   ↓
2. LcoPaymentCalc.R → Payment requirements
```

### Trend Analysis Chain
```
1. Historical Customer Data (monthly accumulation)
   ↓
2. Subscriber_Trend_analysis.R → PNG visualizations
   ↓
3. Forecasting.R → Future projections
```

---

## Recommendations

### 1. **Immediate Actions (High Priority)**

1. **Remove Inline CAS System ID Duplicates**
   - All scripts should use `Functions.R::mq_active_report()`
   - **Affected:** 10+ scripts with inline duplication

2. **Consolidate Retrack Scripts**
   - Merge into single script: `retrack_operations.R`
   - Add parameter: `date_offset` (0=today, 1=tomorrow, etc.)
   - **Saves:** 2-3 redundant scripts

3. **Create Shared Utility Functions in Functions.R**
   - `merge_4week_snapshots(week7, week14, week21, week28)`
   - `export_by_lco(df, lco_col, output_dir, file_prefix)`
   - `filter_by_renewal_date(df, date_col, offset_days)`
   - `read_excel_filtered(file, sheet_pattern, skip_rows)`

### 2. **Medium Priority**

4. **Standardize Wallet Processing**
   - Create: `preprocess_wallet(wallet_df)` in Functions.R
   - Returns: Cleaned, standardized wallet with common filters applied
   - **Affected:** 3 wallet scripts

5. **Create Data Loading Module**
   - `load_mq_active()` - Already exists ✓
   - `load_google_sheets(url)`
   - `load_package_details()`
   - `load_lco_master()`

### 3. **Long-Term Improvements**

6. **Modularize testbench.R**
   - Extract reusable patterns into separate utility scripts
   - Keep testbench for true experimentation only

7. **Create Documentation**
   - Add header comments to each script with:
     - Purpose
     - Inputs required
     - Outputs generated
     - Last updated date
     - Dependencies

8. **Version Control for Deprecated Scripts**
   - Move obsolete scripts to `Source/Archive/`
   - Example: `pmrreportobsolete.R`

---

## Quick Search Tips

### Find by Task
Use Ctrl+F on these keywords:

- **MSR Report**: Search for "MSR" → `ListActivetoMSR.R`, `IPTVMSR_v2.R`
- **Wallet**: Search for "Wallet" → `Wallet_analysis.R`, `Wallet_reconcile.R`
- **Broadcaster**: Search for "Broadcaster" or "BC_" → `BC_Report.R`, `Broadcaster_wallet.R`
- **Reconciliation**: Search for "Reconcil" → `Reconcilliation.R`, `Gospell_command_eval.R`
- **Active Customer**: Search for "Active" → `active_analysis.R`, `ActiveCustomer.R`
- **API**: Search for "retrack" or "API" → `retrack_*.R`, `resendBose_*.R`
- **Forecasting**: Search for "Forecast" or "Trend" → `Forecasting.R`, `Subscriber_Trend_analysis.R`

### Find by Code Pattern
- **Pro-rata calculation**: See Code Snippet Library § 5
- **4-week merge**: See Code Snippet Library § 3
- **CAS system ID**: See Code Snippet Library § 2
- **Export loop**: See Code Snippet Library § 7
- **Pivot table**: See Code Snippet Library § 3

---

## Version History

**v1.0** - 2025-12-09 - Initial index creation with 59 scripts analyzed

---

**End of Index**