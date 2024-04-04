
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
  arrange(trip,TIME_PER_HOUR) %>%
  select(-PT_TYPE) %>%
  select(-TOTAL_TRIPS) %>%
  select(-YEAR_MONTH) %>%
  select(-ORIGIN_PT_CODE) %>%
  select(-DESTINATION_PT_CODE)

testing = testing %>%
  arrange(trip,TIME_PER_HOUR) %>%
  select(-PT_TYPE) %>%
  select(-TOTAL_TRIPS) %>%
  select(-YEAR_MONTH) %>%
  select(-ORIGIN_PT_CODE) %>%
  select(-DESTINATION_PT_CODE)

# Initialize model
m <- prophet()

# Add hourly seasonality
m <- add_seasonality(m, name='hourly', period=20, fourier.order=3)


# Add additional regressors
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

#write_xlsx(forecast, "../PV Data [Buses and Trains]/Trains/By Train Station/forecast_prophet.xlsx")



# =========== failed run ===============
testing <- read.csv("../PV Data [Buses and Trains]/Trains/By Train Station/transport_node_train_202402.csv")
node_202401 = read.csv("../PV Data [Buses and Trains]/Trains/By Train Station/transport_node_train_202401.csv")
node_202312 = read.csv("../PV Data [Buses and Trains]/Trains/By Train Station/transport_node_train_202312.csv")

training = rbind(node_202312,node_202401)

# change class of date to Date
training = training %>%
  mutate(YEAR_MONTH = as.Date(paste(YEAR_MONTH,"01",sep = "-"))) 

testing = testing %>%
  mutate(YEAR_MONTH = as.Date(paste(YEAR_MONTH,"01",sep = "-")))

training$ds = training$YEAR_MONTH
testing$ds = testing$YEAR_MONTH

# Combine date and hour
training$ds <- as.POSIXct(paste(training$ds, training$TIME_PER_HOUR), format = "%Y-%m-%d %H")
testing$ds = as.POSIXct(paste(testing$ds, testing$TIME_PER_HOUR), format = "%Y-%m-%d %H")

training_weekday = training %>%
  select(-PT_TYPE) %>%
  select(-YEAR_MONTH) %>%
  select(-TOTAL_TAP_OUT_VOLUME) %>%
  filter(DAY_TYPE == "WEEKDAY") %>%
  select(-DAY_TYPE) %>%
  rename(y = TOTAL_TAP_IN_VOLUME) %>%
  arrange(PT_CODE,TIME_PER_HOUR) %>%
  mutate(PT_CODE = as.numeric(factor(PT_CODE)))%>%
  select(-TIME_PER_HOUR)

testing_weekday = testing %>%
  select(-PT_TYPE) %>%
  select(-YEAR_MONTH) %>%
  select(-TOTAL_TAP_OUT_VOLUME) %>%
  filter(DAY_TYPE == "WEEKDAY") %>%
  select(-DAY_TYPE) %>%
  arrange(PT_CODE,TIME_PER_HOUR) %>%
  rename(y = TOTAL_TAP_IN_VOLUME) %>%
  mutate(PT_CODE = as.numeric(factor(PT_CODE)))%>%
  select(-TIME_PER_HOUR)

testing_weekend = testing %>%
  select(-PT_TYPE) %>%
  select(-YEAR_MONTH) %>%
  select(-TOTAL_TAP_OUT_VOLUME) %>%
  filter(DAY_TYPE == "WEEKENDS/HOLIDAY") %>%
  select(-DAY_TYPE) %>%
  arrange(PT_CODE,TIME_PER_HOUR) %>%
  rename(y = TOTAL_TAP_IN_VOLUME) %>%
  mutate(PT_CODE = as.numeric(factor(PT_CODE)))%>%
  select(-TIME_PER_HOUR)

training_weekend = training %>%
  select(-PT_TYPE) %>%
  select(-YEAR_MONTH) %>%
  select(-TOTAL_TAP_OUT_VOLUME) %>%
  filter(DAY_TYPE == "WEEKENDS/HOLIDAY") %>%
  select(-DAY_TYPE) %>%
  arrange(PT_CODE,TIME_PER_HOUR) %>%
  rename(y = TOTAL_TAP_IN_VOLUME) %>%
  mutate(PT_CODE = as.numeric(factor(PT_CODE)))%>%
  select(-TIME_PER_HOUR)

# Initialize model
m_weekday <- prophet()
m_weekday <- add_seasonality(m_weekday, name='hourly', period=20, fourier.order=3)
# Add additional regressors
m_weekday <- add_regressor(m_weekday, 'PT_CODE')
# Fit the model
m_weekday <- fit.prophet(m_weekday, training_weekday)


# Create future dataframe
future_weekday <- data.frame(ds_weekday = testing_weekday$ds)
future_weekday$PT_CODE <- testing_weekday$PT_CODE

# Predict
forecast <- predict(m_weekday, future_weekday)

# Calculate RMSE
rmse <- sqrt(mean((testing_weekday$y - forecast$yhat)^2))
print(paste("Root Mean Squared Error: ", rmse))



