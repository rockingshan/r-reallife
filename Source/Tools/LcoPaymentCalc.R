# =============================================================================
# LCO Renewal Pricing Script
# Purpose : Calculate service package costs for subscribers whose contracts
#           fall within a given date window, to generate renewal requirements.
# Author  : GTNJ Studios
# Updated : 2026-06
# =============================================================================
#
# ── INPUT FILES (4 CSVs, prompted in order) ──────────────────────────────────
#
#  1. listActive  ──  Active subscriber list export from your billing/CAS system.
#     Required columns (position matters for the rename below):
#       Col 10  →  VC    (VC / Smart Card number)
#       Col 11  →  STB   (STB / Set-Top Box serial number)
#     Other expected columns used:
#       CUSTOMER_NBR, CONTRACT_NUMBER, ENTITY_CODE, ENTITY_NAME,
#       SERVICE_CODE, SERVICE_NAME, BILLING_FREQUENCY
#
#  2. dueRenewal  ──  Contract end-date report.
#     Expected columns:
#       Contract.Number   (integer contract ID)
#       Contract.End.Date (date string in DD/MM/YYYY format)
#
#  3. LcoPricing  ──  LCO plan pricing master from the headend/billing system.
#     Expected columns:
#       Entity.Code, Lco.Price.Status, Plan.Name, Plan.Category,
#       Plan.Code, Service.Code, Price
#
#  4. renewReq  ──  Manually prepared renewal-days mapping.
#     This file bridges contract numbers to the number of days being renewed.
#     Expected columns:
#       CONTRACT_NUMBER   (integer, must match CONTRACT_NUMBER in listActive)
#       Days              (integer, number of renewal days for that contract)
#     How to prepare: after running step 5 (CUSTOMERS_END_BY_DATE.CSV export),
#     open that output, add a 'Days' column, fill in the renewal period for
#     each customer (e.g. 30, 60, 90), and save as a new CSV.
#
# ── OUTPUT FILES ─────────────────────────────────────────────────────────────
#
#  • CUSTOMERS_END_BY_DATE.CSV  ─  Distinct customers (CUSTOMER_NBR +
#    BILLING_FREQUENCY) whose contracts end in the specified date range.
#    Use this as the basis for preparing renewReq (input file 4).
#
#  • LCO_Price_required_byDate.csv  ─  Final pricing output: one row per
#    service subscription, showing base amount and GST-inclusive amount.
#
# =============================================================================

library(tidyverse)
library(dplyr)
library(lubridate)

# =============================================================================
# PARAMETERS  –  edit these before each run
# =============================================================================

lcoCode  <- 'MD0524'       # LCO entity code to filter on
end_date <- "2026-06-30"   # Mandatory: last contract end date to include (YYYY-MM-DD)
start_date <- "2026-06-01"         # Optional: first contract end date to include (YYYY-MM-DD)
                           #           set to NULL to include everything up to end_date

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# ── 1a. Active subscriber list ───────────────────────────────────────────────
listActive <- read.csv(file.choose())
colnames(listActive)[10] <- "VC"
colnames(listActive)[11] <- "STB"

# Ensure CONTRACT_NUMBER is integer (billing exports sometimes carry it as char)
listActive$CONTRACT_NUMBER <- as.integer(listActive$CONTRACT_NUMBER)

# ── 1b. Contract end-date report ─────────────────────────────────────────────
dueRenewal <- read.csv(file.choose()) %>%
  select(Contract.Number, Contract.End.Date)

dueRenewal$Contract.Number  <- as.integer(dueRenewal$Contract.Number)
dueRenewal$Contract.End.Date <- as.Date(dueRenewal$Contract.End.Date, "%d/%m/%Y")

# ── 1c. LCO pricing master ───────────────────────────────────────────────────
LcoPricing <- read.csv(file.choose())


# =============================================================================
# 2. BUILD LCO PRICING LOOKUP
# =============================================================================

LcoPricingFilter <- LcoPricing %>%
  filter(Entity.Code %in% lcoCode) %>%
  filter(Lco.Price.Status == 'A') %>%
  filter(!(Plan.Name == 'Alacarte (Promotional)'))

# For Alacarte plans the unique identifier is Service.Code, not Plan.Code
LcoPricingFilter$Plan.Code[LcoPricingFilter$Plan.Category == 'Alacarte'] <-
  LcoPricingFilter$Service.Code[LcoPricingFilter$Plan.Category == 'Alacarte']

LCoFinalPricing <- LcoPricingFilter %>%
  select(Plan.Code, Price)

# Normalise FTA plan code naming convention
LCoFinalPricing$Plan.Code <- gsub("FTAPLAN", "FTA", LCoFinalPricing$Plan.Code)

# =============================================================================
# 3. FILTER ACTIVE SUBSCRIBERS FOR THIS LCO
# =============================================================================

listActiveFilter <- listActive %>%
  select(CUSTOMER_NBR, CONTRACT_NUMBER, ENTITY_CODE, ENTITY_NAME,
         STB, VC, SERVICE_CODE, SERVICE_NAME, BILLING_FREQUENCY) %>%
  filter(ENTITY_CODE %in% lcoCode) %>%
  filter(!(SERVICE_CODE == 'DPOBUNDLESERV')) %>%   # exclude bundle wrapper rows
  unique()

# =============================================================================
# 4. FILTER CONTRACTS BY DATE RANGE
# =============================================================================

end_dt   <- as.Date(end_date)
start_dt <- if (!is.null(start_date)) as.Date(start_date) else as.Date("1900-01-01")

dueRenewalFilter <- dueRenewal %>%
  filter(Contract.End.Date >= start_dt & Contract.End.Date <= end_dt)

# =============================================================================
# 5. MERGE ACTIVE LIST WITH DATE-FILTERED CONTRACTS
# =============================================================================

listActivewithEndDate <- merge(
  listActiveFilter,
  dueRenewalFilter,
  by.x = 'CONTRACT_NUMBER',
  by.y = 'Contract.Number'
)

# ── Intermediate export: hand this to the ops team to fill in renewal days ───
write.csv(
  listActivewithEndDate %>%
    select(CUSTOMER_NBR, BILLING_FREQUENCY) %>%
    unique(),
  "CUSTOMERS_END_BY_DATE.CSV",
  row.names = FALSE
)

message("Exported CUSTOMERS_END_BY_DATE.CSV — add a 'Days' column and save as renewReq input (file 4).")

# =============================================================================
# 6. ATTACH PLAN PRICES
# =============================================================================

LcoPackWithPrice <- merge(
  listActivewithEndDate,
  LCoFinalPricing,
  by.x = 'SERVICE_CODE',
  by.y = 'Plan.Code',
  all.x = TRUE   # keep rows even if no price match (Price will be NA — review these)
)

# =============================================================================
# 7. CALCULATE RENEWAL AMOUNTS
# =============================================================================
# ── 1d. Renewal-days mapping (see documentation at top for how to prepare) ───
renewReq <- read.csv(file.choose())
#renewReq$CONTRACT_NUMBER <- as.integer(renewReq$CONTRACT_NUMBER)

LcoPackWithPriceFinal <- merge(LcoPackWithPrice, renewReq)

LcoPackWithPriceFinal <- LcoPackWithPriceFinal %>%
  mutate(
    FullAmnt  = (Price / 30) * Days,          # pro-rated base amount
    FinalAmnt = FullAmnt * 1.18               # base + 18% GST
  )

# =============================================================================
# 8. FINAL EXPORT
# =============================================================================

write.csv(LcoPackWithPriceFinal, "LCO_Price_required_byDate.csv", row.names = FALSE)

message("Done. Output written to LCO_Price_required_byDate.csv")