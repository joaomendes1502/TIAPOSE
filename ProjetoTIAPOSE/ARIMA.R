library(forecast)

# Read the Walmart sales data:
d <- read.table("walmart.csv", sep = ",", header = TRUE)

# Delete missing values:
d <- na.omit(d)

# Select relevant attributes (assuming the sales column is named "Sales"):
d <- d[, c("WSDep1)]

cat("Full dataset with:", nrow(d), "x", ncol(d), "\n")

# Time series analysis with ARIMA model:
library(forecast)

# Convert sales data to time series object:
ts_data <- ts(d$Sales, frequency = 4)

# Fit ARIMA model:
arima_model <- auto.arima(ts_data)

# Summary of the ARIMA model:
print(summary(arima_model))

# Forecast sales using the ARIMA model:
forecast_values <- forecast(arima_model, h = 30)  # Forecasting for next 30 days

# Plotting forecasted values:
plot(forecast_values, main = "ARIMA Forecast for Walmart Sales")

# Displaying forecasted values:
print(forecast_values)



