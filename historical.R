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


###lco master 
lco_master = read.csv("4153800_LCOMasterReport.CSV")
lco_master$Lco.Code <- gsub("'","",lco_master$Lco.Code)
lco_master <- lco_master[!(is.na(lco_master$City) | lco_master$City==""),] #remove blank values and NA
lco_master_req = lco_master %>% select(Lco.Code,Business.Name,Addrline1,State,City)
city.list = lco_master %>% select(City) %>% unique()
city.list = city.list[['City']]
for (city in city.list) {
  lco_master_filt = filter(lco_master, City==city)
  write.csv(lco_master_filt, paste(city,".csv", sep = ""), row.names = FALSE)
}
write.csv(lco_master_req,"LcoMaster.csv",row.names = F)

lco_details = merge(x = lco_master_req, y = LCODATA, by = "Lco.Code", all.x = F)
write.csv(lco_details,"LCO Details.csv",)