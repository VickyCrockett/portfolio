# Demo file to load and test the model
# Gives tomorrow's forecast
# Real client data gives error within margin they've signed off - this repo uses demo_data rather than the original.
# Model card not included in this portfolio to avoid information breach.

# Load necessary libraries
library(prophet)
library(dplyr)
library(lubridate)

# Load the model from the file
best_model <- readRDS("best_model.rds")

# Create a dataframe with the dates for which you want to make predictions
# Assuming you want to predict for tomorrow
future_dates <- data.frame(ds = Sys.Date() + 1)

# Use the model to make a forecast
forecast <- predict(best_model, future_dates)

# View the forecast
print(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
