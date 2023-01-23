library(tidyverse)
library(lubridate)

payment_data <- read.csv(file.choose())

##convert MQ datetime to date only
payment_data$Payment.Date <- parse_date_time(payment_data$Payment.Date, orders = "dmy HMS")
payment_data$Payment.Date <- as.Date(payment_data$Payment.Date)

payment_data <- payment_data %>% 
  mutate(Payment_Period = ifelse(day(Payment.Date) <= 15, "1st to 15th", "16th to 30th"))

payment_trends <- payment_data %>% 
  group_by(City, Payment_Period) %>% 
  summarize(Total_Payment = sum(Amount))

ggplot(payment_trends, aes(x=City, y=Total_Payment, fill=Payment_Period)) +
  geom_bar(stat="identity", position="dodge") +
  ggtitle("Entities Payment Trends") +
  xlab("City") +
  ylab("Total Payment") +
  scale_y_continuous(labels = scales::comma)
