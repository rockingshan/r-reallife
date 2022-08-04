library(tidyverse)



customer = read_csv(file.choose(new = F),
                    col_types = list(col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_skip(),
                                     col_skip(),col_skip(),col_character(),col_character(),col_character(),col_character(),col_character(),col_skip(),col_character(),
                                     col_character(),col_character(),col_character(),col_skip(),col_skip(),col_skip(),col_skip(),col_skip(),col_skip()))
