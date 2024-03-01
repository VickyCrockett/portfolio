# Client data in Power BI replicated here with random values to create demo_data

library(lubridate)

# Set the start and end dates for the 2-year period
end_date <- Sys.Date()
start_date <- end_date %m-% years(2)

# Create a sequence of dates from start to end
date_sequence <- seq.Date(start_date, end_date, by="day")

# Create a sequence of day indexes (x values)
day_indexes <- 1:length(date_sequence)

# Calculate Received values based on the equation y = 0.05x + 25 + noise
# Assuming 'noise' is normally distributed with mean 0 and standard deviation 5
noise <- rnorm(length(date_sequence), mean = 0, sd = 5)
received_values <- 0.05 * day_indexes + 25 + noise

# Create a data frame
demo_data <- data.frame(Date = date_sequence, Received = received_values)

# View the first few rows of the dataset and plot
head(demo_data)
plot(demo_data)
