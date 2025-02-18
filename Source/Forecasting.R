library(readr)
library(forecast)
library(ggplot2)
library(prophet)

# Load time series data
ts_data <- ts(AirPassengers, start=c(1949,1), frequency=12)

# Apply Simple Exponential Smoothing
ses_model <- ses(ts_data, h=12)

# Plot Forecast
autoplot(ses_model)


###Linear regression
# Load dataset
data <- mtcars

# Fit a linear regression model (predicting mpg based on horsepower)
lm_model <- lm(mpg ~ hp, data=data)

# Print model summary
summary(lm_model)

# Predict for new values
new_data <- data.frame(hp = c(100, 150, 200))
predict(lm_model, newdata=new_data)


##try with Holts method on sub base to predict

# Load dataset
customer_data <- read_csv(file.choose()) ##better to have time wise data to import yyyy-mm-dd
# Convert the Month column to Date format (if needed)
customer_data$Month <- as.Date(paste0(customer_data$Month, "-01"))
# Convert to a time series object
ts_customers <- ts(customer_data$Customers, 
                   start = c(2023, 12),  # Assuming data starts in Jan 2020
                   frequency = 12)      # Monthly data (12 periods per year)
# Check the structure of the time series
print(ts_customers)

# Plot the time series data
autoplot(ts_customers) + 
  ggtitle("Monthly Customer Numbers") +
  xlab("Year") + ylab("Number of Customers")

# Apply Holt’s Method
holt_model <- holt(ts_customers, h = 12)  # Forecast for next 12 months

# Plot the forecast
autoplot(holt_model) + 
  ggtitle("Holt’s Forecast for Customer Numbers") +
  xlab("Year") + ylab("Predicted Customers")

# Manually set alpha (level smoothing) and beta (trend smoothing)
holt_tuned <- holt(ts_customers, h = 12, alpha = 0.7651, beta = 0.19987)

# Plot the tuned forecast
autoplot(holt_tuned) + ggtitle("Tuned Holt’s Method Forecast")

# Print the forecasted values
print(holt_model)

# Extract predicted numbers
future_customers <- holt_model$mean
print(future_customers)

