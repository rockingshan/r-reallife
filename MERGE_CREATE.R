library(tidyverse)
library(here)
library(dplyr)
library(gapminder) 
library(readxl)

cas_codes = read_excel("MQ Service and provision codes.xls")
price_plan = read_excel("81032.xls", col_types = c("guess","guess","guess","guess","guess","guess","guess","text","numeric","numeric"))

try = merge(x=price_plan,y=cas_codes,by="SERVICE_CODE", all.x = T, all.y = TRUE,)

write.csv(try, "PRICE_PLAN_CAS_CODES.CSV", row.names = FALSE)