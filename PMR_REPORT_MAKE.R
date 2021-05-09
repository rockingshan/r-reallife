library(tidyverse)
library(dplyr)
library(readxl)

bC_bouqet_raw = read_excel(choose.files(default = "_STATE_BOUQUET_RPT",caption = "Select Broadcaster report Bouquet File",multi = FALSE,), skip = 3)
bc_alacarte_raw = read_excel(choose.files(default = "_STATE_ALACARTE_RPT",caption = "Select Broadcaster report Alacarte File",multi = FALSE,), skip = 3)
names(bC_bouqet_raw) = make.names(names(bC_bouqet_raw))
names(bc_alacarte_raw) = make.names(names(bc_alacarte_raw))
