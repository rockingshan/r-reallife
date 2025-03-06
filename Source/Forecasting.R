library(readr)
library(forecast)
library(ggplot2)
library(prophet)
library(tidyverse)

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

###using prophet library

# Load dataset
customer_data <- read_csv(file.choose())

# Convert Month column to Date format (YYYY-MM-DD)
customer_data <- customer_data %>%
  rename(ds = Month, y = Customers) %>%   # Prophet needs 'ds' and 'y' column names
  mutate(ds = as.Date(paste0(ds, "-01"))) # Ensure proper date format
# Initialize Prophet model
model <- prophet()

# Fit model to data
model <- fit.prophet(model, customer_data)

# Create future 12-month period
future <- make_future_dataframe(model, periods = 12, freq = "month")

# View future dates
print(future)

# Predict future values
forecast <- predict(model, future)

# View forecasted values
head(forecast[, c("ds", "yhat", "yhat_lower", "yhat_upper")])

# Plot the forecast
plot(model, forecast) + ggtitle("Prophet Forecast for Customer Numbers")

# Plot trend and seasonal components
prophet_plot_components(model, forecast)

##testing model acquracy
# Split data into training (first part) and testing (last 12 months)
train <- customer_data[1:(nrow(customer_data)-12), ]
test <- customer_data[(nrow(customer_data)-11):nrow(customer_data), ]

# Train model on training data
model_train <- prophet()
model_train <- fit.prophet(model_train, train)

# Create future dates for testing period
future_test <- make_future_dataframe(model_train, periods = 12, freq = "month")

# Predict values for test set
forecast_test <- predict(model_train, future_test)

# Compute error metrics
library(Metrics)

mae_val <- mae(test$y, forecast_test$yhat[1:12]) 
rmse_val <- rmse(test$y, forecast_test$yhat[1:12])
mape_val <- mape(test$y, forecast_test$yhat[1:12])

print(paste("MAE:", mae_val))
print(paste("RMSE:", rmse_val))
print(paste("MAPE:", mape_val, "%"))
