library(tidyverse)
library(dplyr)

alachannel = read.csv(file.choose(), colClasses = c(CasCode="character"))
# Pivot the dataframe
pivoted_alacode <- alachannel %>%
  pivot_wider(names_from = CAS, values_from = CasCode)

dpopack = read.csv(file.choose(), colClasses = c(CasCode="character"))
pivoted_dpo <- dpopack %>%
  pivot_wider(names_from = CAS, values_from = CasCode)

broadbqcode = read.csv(file.choose(), colClasses = c(CasCode="character"))
pivoted_brbq <- broadbqcode %>%
  pivot_wider(names_from = CAS, values_from = CasCode)

alabrBQCodes = rbind(pivoted_alacode,pivoted_brbq)


packageConfig = read.csv(file.choose())
packageConfigDPOCode = merge(packageConfig,pivoted_dpo,by.x = "Plan.Name",by.y = "Plan",all.x = T)
preferred_order <- c(2, 1, 5, 6, 7, 8, 3, 4)  # Example column positions
# Reorder the columns
packageConfigDPOCode <- packageConfigDPOCode[, preferred_order]

packDpoBouqChnlCodes = merge(packageConfigDPOCode,alabrBQCodes,by.x = "Bouquet",by.y = "BouquetChannel",all.x = T)
preferred_order <- c(2, 3, 4, 5, 6, 7, 1, 8, 12, 10, 9, 11)  # Example column positions
packDpoBouqChnlCodes <- packDpoBouqChnlCodes[, preferred_order]
write.csv(packDpoBouqChnlCodes,"Output/28042024.csv",row.names = F )
