library(tidyverse)
library(dplyr)
library(readxl)
library(purrr)

list_bouquet_dated = read.csv(file.choose(new = F)) #import MQ data bouquet
list_alacarte = read.csv(file.choose(new = F)) #import MQ alacarte details