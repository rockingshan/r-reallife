library(tidyverse)
library(here)
library(dplyr)

total_cust = read.csv("3941783_CUSTOMERS_NETWORK.CSV")
names(total_cust) = make.names(names(total_cust))
total_cust_stb = total_cust %>% select(Customer.Nbr,STB.Item.Descr) %>% distinct()
total_cust_stb = total_cust_stb %>% group_by(STB.Item.Descr) %>% 
  summarize(CustomerCount = n())
total_cust_stb <- total_cust_stb[order(-total_cust_stb$CustomerCount),]
write.csv(total_cust_stb, "STBwise Customer.CSV", row.names = FALSE)