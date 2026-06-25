library(tidyverse)
library(dplyr)
library(readxl)
library(openxlsx)
library(stringr)
library(janitor)

# ── INPUT FILES ────────────────────────────────────────────────────────────────
cat("Paste path for SMS Report (IPTV all customer report): ")
SMS_REPORT_PATH <- trimws(readLines(con = stdin(), n = 1))

cat("Paste path for DRM Report (Export STB from DRM): ")
DRM_REPORT_PATH <- trimws(readLines(con = stdin(), n = 1))
# ──────────────────────────────────────────────────────────────────────────────

smsReport <- readxl::read_xlsx(SMS_REPORT_PATH, skip = 1)
drmReport <- readxl::read_xlsx(DRM_REPORT_PATH)

# packConfig <- read_xlsx(PACK_CONFIG_PATH, skip = 2)
# packConfig <- janitor::clean_names(packConfig)

## Clean column names
smsReport  <- janitor::clean_names(smsReport)
drmReport  <- janitor::clean_names(drmReport)

# packCodes  <- packConfig %>% select(package_name, plan_code) %>% unique()
# colnames(packCodes)[1] <- "video_plan"
# smsReportMod <- merge(smsReport, packCodes, all.x = TRUE)

drmReportMod <- drmReport %>%
  select(cas_serial_number, signature, packages, status)

## Normalise status labels
drmReportMod$status       <- gsub("a", "Active",   drmReportMod$status)
drmReportMod$status       <- gsub("d", "Inactive", drmReportMod$status)
smsReport$customer_status <- gsub("Suspended", "Inactive", smsReport$customer_status)

## Build combined key columns
smsReport    <- smsReport    %>% unite(combined, c("userid", "video_plan_code", "customer_status"),   sep = "|", remove = FALSE)
drmReportMod <- drmReportMod %>% unite(combined, c("cas_serial_number", "packages", "status"), sep = "|", remove = FALSE)

## Merge and filter mismatches
finalData <- merge(smsReport, drmReportMod, by.x = "userid", by.y = "cas_serial_number", all.x = TRUE)
nf <- finalData[finalData[, 3] != finalData[, 16], ]   # mismatch between combined columns
nf <- nf %>% filter(!is.na(userid))

## Write output
dir.create("Output", showWarnings = FALSE)

write.csv(nf, "Output/IPTV_reconcile.csv", row.names = FALSE)
write.xlsx(nf, "Output/IPTV_reconcile.xlsx", rowNames = FALSE)

cat("Done! Output written to Output/IPTV_reconcile.csv and .xlsx\n")