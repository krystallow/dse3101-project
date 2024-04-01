
library(tidyverse)
library(lubridate)
library(prophet)

# Passenger Volume for Trains - By Origin Destination (2023 Dec)
pv_train_od_202312 <- read.csv("../PV Data [Buses and Trains]/Trains/By OD/origin_destination_train_202312.csv")
#pv_train_od_202312 = pv_train_od_202312 %>%
#  mutate(month = substring(YEAR_MONTH,6,7))

# Passenger Volume for Trains - By Origin Destination (2024 Jan)
pv_train_od_202401 <- read.csv("../PV Data [Buses and Trains]/Trains/By OD/origin_destination_train_202401.csv")
#pv_train_od_202401 = pv_train_od_202401 %>%
#  mutate(month = substring(YEAR_MONTH,6,7))

# Passenger Volume for Trains - By Origin Destination (2024 Feb) TESTING!
pv_train_od_202402 <- read.csv("../PV Data [Buses and Trains]/Trains/By OD/origin_destination_train_202402.csv")
#pv_train_od_202402 = pv_train_od_202402 %>%
#  mutate(month = substring(YEAR_MONTH,6,7))

# combine data from Dec 2023 - Feb 2024 of passenger volume for trains with origin destinations
pv_train_od = rbind(pv_train_od_202312,pv_train_od_202401)
testing = pv_train_od_202402

# change class of date to Date
pv_train_od = pv_train_od %>%
  mutate(YEAR_MONTH = as.Date(paste(YEAR_MONTH,"01",sep = "-"))) %>%
  mutate(trip = str_c(ORIGIN_PT_CODE, DESTINATION_PT_CODE, sep="-"))

testing = testing %>%
  mutate(YEAR_MONTH = as.Date(paste(YEAR_MONTH,"01",sep = "-"))) %>%
  mutate(trip = str_c(ORIGIN_PT_CODE, DESTINATION_PT_CODE, sep="-"))

pv_train_od$ds = pv_train_od$YEAR_MONTH
testing$ds = testing$YEAR_MONTH
# Combine date and hour
pv_train_od$ds <- as.POSIXct(paste(pv_train_od$ds, pv_train_od$TIME_PER_HOUR), format = "%Y-%m-%d %H")
testing$ds = as.POSIXct(paste(testing$ds, testing$TIME_PER_HOUR), format = "%Y-%m-%d %H")

#converting everything to numeric for Prophet
pv_train_od$y = pv_train_od$TOTAL_TRIPS
pv_train_od$trip <- as.numeric(factor(pv_train_od$trip))
pv_train_od$ORIGIN_PT_CODE <- as.numeric(factor(pv_train_od$ORIGIN_PT_CODE))
pv_train_od$DESTINATION_PT_CODE <- as.numeric(factor(pv_train_od$DESTINATION_PT_CODE))
pv_train_od$DAY_TYPE <- as.numeric(factor(pv_train_od$DAY_TYPE))

testing$y = testing$TOTAL_TRIPS
testing$trip <- as.numeric(factor(testing$trip))
testing$ORIGIN_PT_CODE <- as.numeric(factor(testing$ORIGIN_PT_CODE))
testing$DESTINATION_PT_CODE <- as.numeric(factor(testing$DESTINATION_PT_CODE))
testing$DAY_TYPE <- as.numeric(factor(testing$DAY_TYPE))


pv_train_od = pv_train_od %>%
  select(-PT_TYPE) %>%
  select(-TOTAL_TRIPS) %>%
  select(-YEAR_MONTH) %>%
  select(-ORIGIN_PT_CODE) %>%
  select(-DESTINATION_PT_CODE)

testing = testing %>%
  select(-PT_TYPE) %>%
  select(-TOTAL_TRIPS) %>%
  select(-YEAR_MONTH) %>%
  select(-ORIGIN_PT_CODE) %>%
  select(-DESTINATION_PT_CODE)

# Initialize model
m <- prophet()

# Add hourly seasonality
m <- add_seasonality(m, name='hourly', period=24, fourier.order=3)


# Add additional regressors
#m <- add_regressor(m, 'month')
m <- add_regressor(m, 'DAY_TYPE')
m <- add_regressor(m, 'trip')

# Fit the model
m <- fit.prophet(m, pv_train_od)


# Create future dataframe
future <- data.frame(ds = testing$ds)

future$DAY_TYPE <- testing$DAY_TYPE
future$trip <- testing$trip

# Predict
forecast <- predict(m, future)

# Calculate RMSE
rmse <- sqrt(mean((testing$y - forecast$yhat)^2))
print(paste("Root Mean Squared Error: ", rmse))

# Plot the forecast
prophet_plot_components(m, forecast)


write.csv(forecase, file = "prophet_forecasts.csv")


# Make valid names for the column names
#names(pv_train_od) <- make.names(names(pv_train_od), unique = TRUE)
#names(origin_destination_dummies) <- make.names(names(origin_destination_dummies), unique = TRUE)
#names(final_destination_dummies) <- make.names(names(final_destination_dummies), unique = TRUE)



# Now you can add the dummy variables as extra regressors
#dummy_cols <- colnames(origin_destination_dummies)
#for (col in dummy_cols) {
  #model <- add_regressor(model, col)
#}


# Fit the model
#model <- fit.prophet(model, pv_train_od)

#future <- make_future_dataframe(model, periods = 365)
#future$is_weekday <- ifelse(future$DAY_TYPE == 'WEEKDAY', 1, 0)
#forecast = predict(model,future)

# Bus Data

#pv_bus_od = rbind(pv_bus_od_202312,pv_bus_od_202401,pv_bus_od_202402)

# change class of date to Date
#pv_bus_od = pv_bus_od %>%
#  mutate(YEAR_MONTH = as.Date(paste(YEAR_MONTH,"01",sep = "-")))

#pv_bus_od$ds = pv_bus_od$YEAR_MONTH
# Combine date and hour
#pv_bus_od$ds <- as.POSIXct(paste(pv_bus_od$ds, pv_bus_od$TIME_PER_HOUR), format = "%Y-%m-%d %H")

#converting everything to numeric for Prophet
#pv_bus_od$y = pv_bus_od$TOTAL_TRIPS
#pv_bus_od$ORIGIN_PT_CODE <- as.numeric(factor(pv_bus_od$ORIGIN_PT_CODE))
#pv_bus_od$DESTINATION_PT_CODE <- as.numeric(factor(pv_bus_od$DESTINATION_PT_CODE))
#pv_bus_od$DAY_TYPE <- as.numeric(factor(pv_bus_od$DAY_TYPE))


#pv_bus_od = pv_bus_od %>%
#  select(-PT_TYPE) %>%
#  select(-TOTAL_TRIPS) %>%
#  select(-YEAR_MONTH)

# Initialize model
#m_bus <- prophet()

# Add additional regressors
#m_bus <- add_regressor(m_bus, 'DAY_TYPE')
#m_bus <- add_regressor(m_bus, 'TIME_PER_HOUR')
#m_bus <- add_regressor(m_bus, 'ORIGIN_PT_CODE')
#m_bus <- add_regressor(m_bus, 'DESTINATION_PT_CODE')
# Fit the model
#m_bus <- fit.prophet(m_bus, pv_bus_od)