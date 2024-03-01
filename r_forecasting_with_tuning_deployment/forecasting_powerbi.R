# This is included here to demonstrate progress so far - full solution pending cost approval
# The hyperparameter tuning below takes >5 mins (Power BI maximum)
# This was the initial prototype - costing for hosting the model on Azure to feed into Power BI currently being considered
# Quote issued to client to deploy the forecast model on Azure ML with MLFlow or vetiver monitoring (prototyping required for lengths of stream or full data with batch updates).
# Also suggested prototyping using Azure Functions (called from PBI still) as that's probably cheaper.
# Further domain information requested in the meantime to see if I can adjust the scanning window to fit most likely scenarios and reduce the number of combinations tried when model retraining.
# Code added at the end to illustrate how to package the model that can then be loaded with the load_model.r script

library(lubridate)
library(prophet)
library(tidyverse)
library(ggplot2)

# Data in Power BI is loaded as dataset (with cleaning and prep records included for data governance)
#time_series <- dataset |> mutate(timestamp = ymd_hms(Date))
time_series <- demo_data |> mutate(timestamp = ymd(Date))
# Replaced with demo_data for github portfolio to protect the client's data.
# Warning: demo_data was randomly generated and therefore no inferences can be made.

time_series <- time_series |> mutate(ds = timestamp, y = Received)

# Define hyperparameter grid
hyperparameters <- expand.grid(
  #holidays = list(NULL, data.frame(holiday = 'holiday_name', ds = ymd('2024-01-01'))),
  seasonality.mode = c('additive', 'multiplicative'),
  seasonality.prior.scale = seq(0.01, 0.2, by = 0.01),
  #holidays.prior.scale = seq(0.01, 0.2, by = 0.01),
  changepoint.prior.scale = seq(0.01, 0.2, by = 0.01),
  changepoint.range = seq(0.8, 0.95, by = 0.01)
)

# Initialize variables to store the best model and its performance
best_model <- NULL
best_mae <- Inf

# Time the loop
start_time <- Sys.time()

# Loop through hyperparameter combinations
for (i in 1:nrow(hyperparameters)) {
  params <- hyperparameters[i, ]
  
  # Create the prophet model with the current hyperparameters
  m <- prophet(time_series,
               #holidays = params$holidays,
               seasonality.mode = params$seasonality.mode,
               seasonality.prior.scale = params$seasonality.prior.scale,
               #holidays.prior.scale = params$holidays.prior.scale,
               changepoint.prior.scale = params$changepoint.prior.scale,
               changepoint.range = params$changepoint.range)
  
  # Make forecasts
  future <- make_future_dataframe(m, periods = 365)
  forecast <- predict(m, future)
  
  # Evaluate the model using MAE (you can use other metrics)
  mae <- mean(abs(forecast$yhat - time_series$y))
  
  # Check if this model is better than the previous best
  if (mae < best_mae) {
    best_mae <- mae
    best_model <- m
    best_params <- params
  }
}

# Finish timing the loop
end_time <- Sys.time()
print(end_time - start_time)
# Result locally (Power BI timed out):  

# Print the best hyperparameters and model
print(best_params)
print(best_model)

# Plot the best forecast
plot(best_model, forecast)

# Save visual for governance record
p <- plot(best_model, forecast)
ggsave("forecast_plot.png", p, width = 10, height = 6) # Client filepath removed

# Packaging model
saveRDS(best_model, "best_model.rds")
