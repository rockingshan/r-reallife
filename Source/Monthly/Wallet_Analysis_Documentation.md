---
output:
  word_document: default
  html_document: default
---
# WALLET REVENUE & CUSTOMER ANALYSIS - OUTPUT FILES DOCUMENTATION

**Script Name:** Wallet_Revenue_Paid_Free_Analysis.R
**Purpose:** Analyze wallet transaction data to understand revenue trends, customer payment behavior, conversion patterns, and geographical performance

**Total Files Generated:** 9 files (5 LCO-level + 4 City-level)

---

## OUTPUT FILES GENERATED

### 1. **Wallet_LCO_Monthly_Wide.csv**

**Purpose:** Month-by-month comparison of all LCOs in a single view

**What it contains:**
- Each LCO (Entity Code and Name) as rows
- For each month, you will see 5 columns:
  - **Total Customers:** Count of all active customers
  - **Paid Customers:** Customers who paid (Amount Debit ≠ 0)
  - **Free Customers:** Customers who didn't pay (Amount Debit = 0)
  - **Total Revenue:** Sum of all payments collected
  - **ARPU:** Average Revenue Per User (Total Revenue ÷ Total Customers)

**When to use:**
- Quick comparison of LCO performance across multiple months
- Identify which LCOs are growing or declining in revenue
- Spot trends in customer counts month-over-month

---

### 2. **Wallet_LCO_Monthly_Details.csv**

**Purpose:** Detailed month-wise breakdown for deeper analysis

**What it contains:**
- Same metrics as File #1, but in long format
- Each row represents one LCO for one month
- Columns: Entity Code, Entity Name, Month, Total_Customers, Paid_Customers, Free_Customers, Total_Revenue, ARPU

**When to use:**
- Detailed analysis of specific months
- Easier to filter and sort by specific criteria
- Better for charts and graphs in Excel/PowerBI

---

### 3. **Wallet_Overall_Monthly_Summary.csv**

**Purpose:** Company-wide monthly performance summary

**What it contains:**
- Overall totals across ALL LCOs combined
- One row per month showing:
  - Total customers across all LCOs
  - Total paid customers
  - Total free customers
  - Total company revenue
  - Company-wide ARPU

**When to use:**
- High-level executive summary
- Track overall company performance trends
- Understand if revenue decline is company-wide or specific to certain LCOs

---

### 4. **Wallet_LCO_Customer_Journey_Summary.csv** ⭐ **KEY FILE**

**Purpose:** Understand customer conversion patterns and revenue sources

**What it contains (for each LCO):**

**Customer Segmentation:**
- **Always_Paid_Count:** Customers who paid from the beginning and never used free service
- **Free_to_Paid_Count:** Customers who started free but later converted to paid
- **Always_Free_Count:** Customers who enjoyed free service throughout and never paid
- **Total_Customers:** All customers

**Percentage Breakdown:**
- **Always_Paid_Pct:** What % of customers are loyal paying customers
- **Free_to_Paid_Pct:** What % successfully converted from free to paid
- **Always_Free_Pct:** What % are freeloaders who never paid
- **Conversion_Rate_Pct:** Success rate of converting free users to paid (Free_to_Paid ÷ (Free_to_Paid + Always_Free) × 100)

**Revenue Analysis:**
- **Total_Revenue_All:** Total revenue from all customers
- **Revenue_from_Converted:** How much money came from converted customers
- **Revenue_from_Always_Paid:** How much money came from loyal paying customers

**When to use:**
- **MOST IMPORTANT FILE for understanding why revenue is declining despite new subscribers**
- Check if new subscribers are mostly free users who never convert
- Identify which LCOs are good at converting free users to paid
- Understand if revenue decline is due to too many free customers or low conversion rates

**Key Questions This File Answers:**
1. Are we adding paying customers or just free users?
2. Which LCOs are effective at converting free trials to paid subscriptions?
3. Where is our revenue actually coming from?

---

### 5. **Wallet_Overall_Customer_Journey_Summary.csv**

**Purpose:** Company-wide conversion summary

**What it contains:**
- Same metrics as File #4, but aggregated across ALL LCOs
- Single-row summary showing overall company conversion performance

**When to use:**
- Executive summary of conversion effectiveness
- Quick answer: "Are we converting free users to paid customers effectively?"
- Board presentations and high-level reports

---

## CITY-LEVEL ANALYSIS FILES

### 6. **Wallet_City_Monthly_Wide.csv**

**Purpose:** Month-by-month comparison of all cities in a single view

**What it contains:**
- Each city as rows
- For each month, you will see 5 columns:
  - **Total Customers:** All customers across all LCOs in the city
  - **Paid Customers:** Customers who paid
  - **Free Customers:** Customers who didn't pay
  - **Total Revenue:** Sum of all payments in the city
  - **ARPU:** Average Revenue Per User for the city

**When to use:**
- Compare city performance across multiple months
- Identify which cities are performing well or poorly
- Understand geographical revenue distribution

---

### 7. **Wallet_City_Monthly_Details.csv**

**Purpose:** Detailed city-wise monthly breakdown

**What it contains:**
- Each row represents one city for one month
- Columns: City, Month, Total_Customers, Paid_Customers, Free_Customers, Total_Revenue, ARPU, LCO_Count
- Shows number of LCOs operating in each city

**When to use:**
- Deep dive into specific city performance
- Track how many LCOs are active in each city
- Better for creating charts and visualizations

---

### 8. **Wallet_City_Revenue_Trend_Analysis.csv** ⭐ **KEY CITY FILE**

**Purpose:** Identify which cities are experiencing revenue growth or decline

**What it contains (for each city):**

**Period Comparison:**
- **First_Month / Last_Month:** Date range analyzed
- **First_Month_Revenue / Last_Month_Revenue:** Revenue at start and end of period
- **First_Month_Customers / Last_Month_Customers:** Customer count at start and end
- **First_Month_ARPU / Last_Month_ARPU:** ARPU at start and end

**Change Analysis:**
- **Revenue_Change:** Absolute change in revenue (Last - First)
- **Revenue_Change_Pct:** Percentage change in revenue
- **Customer_Change:** Absolute change in customer count
- **Customer_Change_Pct:** Percentage change in customers
- **ARPU_Change:** Absolute change in ARPU
- **ARPU_Change_Pct:** Percentage change in ARPU
- **Trend_Status:** Categorized as "Declining" (<-5%), "Growing" (>5%), or "Stable"

**When to use:**
- **CRITICAL for identifying problem cities where revenue is declining**
- Understand if customer growth is translating to revenue growth
- Identify cities where ARPU is falling despite customer additions
- Prioritize cities for management intervention

**Key Questions This File Answers:**
1. Which cities are experiencing revenue decline?
2. Are we adding customers but losing revenue per customer (ARPU decline)?
3. Which cities need immediate attention?

---

### 9. **Wallet_City_Customer_Journey_Summary.csv**

**Purpose:** City-level conversion effectiveness analysis

**What it contains (for each city):**

**Customer Segmentation:**
- **Always_Paid_Count:** Loyal paying customers
- **Free_to_Paid_Count:** Successfully converted customers
- **Always_Free_Count:** Free users who never converted
- **Total_Customers:** All customers

**Performance Metrics:**
- **Always_Paid_Pct, Free_to_Paid_Pct, Always_Free_Pct:** Percentage breakdowns
- **Conversion_Rate_Pct:** Success rate of converting free to paid
- **Total_Revenue, Revenue_from_Converted, Revenue_from_Always_Paid:** Revenue sources

**When to use:**
- Understand which cities are better at converting free users
- Identify cities with too many free users
- Compare conversion strategies across different geographies
- Find best practices from high-performing cities

---

## KEY INSIGHTS TO LOOK FOR

### Understanding Revenue Decline

If **revenue is declining despite adding new subscribers**, check:

**Step 1: Identify Problem Areas Geographically**
1. **File #8 (City Revenue Trend):**
   - Look for cities with **Trend_Status = "Declining"**
   - Check **Revenue_Change_Pct** - negative values indicate revenue loss
   - Check **ARPU_Change_Pct** - negative values mean you're earning less per customer
   - **Key finding:** If customers are growing but revenue is falling, it's an ARPU problem

**Step 2: Understand Customer Quality Issues**
2. **File #4 (LCO Customer Journey):**
   - Is **Always_Free_Count** increasing more than **Always_Paid_Count**?
   - Is **Conversion_Rate_Pct** low (below 20-30%)?
   - This means: New subscribers are free users who aren't converting

3. **File #9 (City Customer Journey):**
   - Which cities have the highest **Always_Free_Pct**?
   - Which cities have low **Conversion_Rate_Pct**?
   - This identifies cities with poor conversion strategies

**Step 3: Validate with Monthly Trends**
4. **File #1 or #2 (Monthly Data):**
   - Is **Free_Customers** count growing while **Paid_Customers** is flat or declining?
   - Is **ARPU** decreasing over time?
   - This means: Customer base quality is deteriorating

5. **File #3 (Overall Summary):**
   - Compare early months vs recent months
   - If Total_Customers grew but Total_Revenue didn't grow proportionally, it confirms the free user problem

### Actionable Decisions

**Good LCO/City Performance Indicators:**
- High Conversion_Rate_Pct (>30%)
- Revenue_from_Converted is significant
- ARPU is stable or increasing
- Trend_Status = "Growing" or "Stable"
- Revenue_Change_Pct is positive

**Poor LCO/City Performance Indicators:**
- High Always_Free_Count with low Free_to_Paid_Count
- Low Conversion_Rate_Pct (<15%)
- Decreasing ARPU over time
- Trend_Status = "Declining"
- Negative Revenue_Change_Pct despite positive Customer_Change_Pct

**Actions to Take:**

**For Cities with Declining Revenue (File #8):**
- Investigate why ARPU is falling (too many free users? pricing issues?)
- Review LCO performance within that city
- Consider city-specific promotional campaigns or pricing adjustments

**For Cities/LCOs with Poor Conversion (Files #4 & #9):**
- Implement stricter free trial policies
- Sales team intervention for free user conversion
- Study successful cities/LCOs and replicate their strategies

**For Overall Strategy:**
- Focus new customer acquisition in high-performing cities
- Consider exiting or restructuring operations in consistently declining cities
- Benchmark best-performing cities and apply learnings across the network

---

## TECHNICAL NOTES

- **City Data Integration:** City information is automatically loaded from LCO Master Report and matched with wallet data by Entity Code
- **Valid Base Plans Only:** Analysis includes only customers with valid base plans from the master list (ALL_PLAN_NAMES_NEWOLD_august25.csv)
- **Date Range:** Script automatically detects and analyzes all months present in the data file
- **Paid vs Free:** Determined by Amount Debit column (0 = Free, >0 = Paid)
- **ARPU Calculation:** Total Revenue ÷ Total Customers (includes both paid and free)
- **Conversion Rate:** Percentage of free users who eventually became paid customers
- **Trend Status:** Cities are categorized as "Declining" if revenue change < -5%, "Growing" if > 5%, otherwise "Stable"
- **LCO Master Report:** Located at `C:\Users\shant\Downloads\MQ report download\16279489_LCOMasterReport.CSV`

---

## FILE ORGANIZATION SUMMARY

**Quick Reference Guide:**

**For Executive Review:**
- Start with File #8 (City Revenue Trend) - identifies problem cities
- Then review File #3 (Overall Monthly Summary) - company-wide trends
- Finally check File #5 (Overall Customer Journey) - conversion effectiveness

**For Detailed Analysis:**
- Files #1, #2, #6, #7 - Month-by-month detailed breakdowns
- Files #4, #9 - Customer journey and conversion analysis by LCO/City

**For Strategic Planning:**
- File #8 - Geographical prioritization (which cities to focus on)
- Files #4, #9 - Conversion optimization opportunities
- Files #6, #7 - City-level resource allocation decisions

---

**For Questions or Issues:** Contact Data Analytics Team
