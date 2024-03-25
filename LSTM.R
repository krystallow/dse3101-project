
library(tidyverse)
library(stringr)
#install.pakcages("keras", "tensorflow")
library(keras)
library(tensorflow)

model <- keras_model_sequential()


# Passenger Volume for Buses - By Bus Stops (2023 Dec)
pv_bus_202312 <- read.csv("../PV Data [Buses and Trains]/Buses/By Bus Stop/transport_node_bus_202312.csv") %>%
  mutate(seasonal_dummy = ifelse(DAY_TYPE == "WEEKENDS/HOLIDAY", 1, 
                                 ifelse(DAY_TYPE == "WEEKDAY", 0, NA)))

# Passenger Volume for Buses - By Bus Stops (2024 Jan)
pv_bus_202401 <- read.csv("../PV Data [Buses and Trains]/Buses/By Bus Stop/transport_node_bus_202401.csv") %>%
  mutate(seasonal_dummy = ifelse(DAY_TYPE == "WEEKENDS/HOLIDAY", 1, 
                                 ifelse(DAY_TYPE == "WEEKDAY", 0, NA)))

# Passenger Volume for Buses - By Bus Stops (2024 Feb)
pv_bus_202402 <- read.csv("../PV Data [Buses and Trains]/Buses/By Bus Stop/transport_node_bus_202402.csv") %>%
  mutate(seasonal_dummy = ifelse(DAY_TYPE == "WEEKENDS/HOLIDAY", 1, 
                                 ifelse(DAY_TYPE == "WEEKDAY", 0, NA)))

# Passenger Volume for Buses - By Origin Destination Bus Stops (2023 Dec)
pv_bus_od_202312 <- read.csv("../PV Data [Buses and Trains]/Buses/By OD/origin_destination_bus_202312.csv") %>%
  mutate(seasonal_dummy = ifelse(DAY_TYPE == "WEEKENDS/HOLIDAY", 1, 
                                 ifelse(DAY_TYPE == "WEEKDAY", 0, NA)))

# Passenger Volume for Buses - By Origin Destination Bus Stops (2024 Jan)
pv_bus_od_202401 <- read.csv("../PV Data [Buses and Trains]/Buses/By OD/origin_destination_bus_202401.csv") %>%
  mutate(seasonal_dummy = ifelse(DAY_TYPE == "WEEKENDS/HOLIDAY", 1, 
                                 ifelse(DAY_TYPE == "WEEKDAY", 0, NA)))

# Passenger Volume for Buses - By Origin Destination Bus Stops (2024 Feb)
pv_bus_od_202402 <- read.csv("../PV Data [Buses and Trains]/Buses/By OD/origin_destination_bus_202402.csv") %>%
  mutate(seasonal_dummy = ifelse(DAY_TYPE == "WEEKENDS/HOLIDAY", 1, 
                                 ifelse(DAY_TYPE == "WEEKDAY", 0, NA)))

# Passenger Volume for Trains - By Train Stops (2023 Dec)
pv_train_202312 <- read.csv("../PV Data [Buses and Trains]/Trains/By Train Station/transport_node_train_202312.csv") %>%
  mutate(seasonal_dummy = ifelse(DAY_TYPE == "WEEKENDS/HOLIDAY", 1, 
                                 ifelse(DAY_TYPE == "WEEKDAY", 0, NA)))


# Passenger Volume for Trains - By Train Stops (2024 Jan)
pv_train_202401 <- read.csv("../PV Data [Buses and Trains]/Trains/By Train Station/transport_node_train_202401.csv") %>%
  mutate(seasonal_dummy = ifelse(DAY_TYPE == "WEEKENDS/HOLIDAY", 1, 
                                 ifelse(DAY_TYPE == "WEEKDAY", 0, NA)))

# Passenger Volume for Trains - By Train Stops (2024 Feb)
pv_train_202402 <- read.csv("../PV Data [Buses and Trains]/Trains/By Train Station/transport_node_train_202402.csv") %>%
  mutate(seasonal_dummy = ifelse(DAY_TYPE == "WEEKENDS/HOLIDAY", 1, 
                                 ifelse(DAY_TYPE == "WEEKDAY", 0, NA)))

# Passenger Volume for Trains - By Origin Destination (2023 Dec)
pv_train_od_202312 <- read.csv("../PV Data [Buses and Trains]/Trains/By OD/origin_destination_train_202312.csv") %>%
  mutate(seasonal_dummy = ifelse(DAY_TYPE == "WEEKENDS/HOLIDAY", 1, 
                                 ifelse(DAY_TYPE == "WEEKDAY", 0, NA)))

# Passenger Volume for Trains - By Origin Destination (2024 Jan)
pv_train_od_202401 <- read.csv("../PV Data [Buses and Trains]/Trains/By OD/origin_destination_train_202401.csv") %>%
  mutate(seasonal_dummy = ifelse(DAY_TYPE == "WEEKENDS/HOLIDAY", 1, 
                                 ifelse(DAY_TYPE == "WEEKDAY", 0, NA)))

# Passenger Volume for Trains - By Origin Destination (2024 Feb)
pv_train_od_202402 <- read.csv("../PV Data [Buses and Trains]/Trains/By OD/origin_destination_train_202402.csv") %>%
  mutate(seasonal_dummy = ifelse(DAY_TYPE == "WEEKENDS/HOLIDAY", 1, 
                                 ifelse(DAY_TYPE == "WEEKDAY", 0, NA)))


combined_train_pv_od <- rbind(pv_train_od_202312, pv_train_od_202401, pv_train_od_202402) %>%
  mutate(trip = str_c(ORIGIN_PT_CODE, DESTINATION_PT_CODE, sep="-")) %>%
  #mutate(YEAR_MONTH = as.Date(paste(YEAR_MONTH,"01",sep = "-"))) %>% 
  mutate(YEAR_MONTH_HOUR = str_c(YEAR_MONTH, TIME_PER_HOUR, sep = "-")) %>%
  #mutate(YEAR_MONTH_HOUR = as.Date(ymd_h(YEAR_MONTH_HOUR))) %>%
  #mutate(YEAR_MONTH_HOUR = as.POSIXct(combined_train_pv_od$YEAR_MONTH_HOUR, format = "%Y-%m-%d %H")) %>%
  #mutate(trip = as.factor(trip)) %>%
  #mutate(seasonal_dummy = as.factor(seasonal_dummy)) %>%
  mutate(ORIGIN_PT_CODE = as.factor(ORIGIN_PT_CODE)) %>%
  mutate(DESTINATION_PT_CODE = as.factor(DESTINATION_PT_CODE))

#combined_train_pv_od$YEAR_MONTH_HOUR <- as.POSIXct(combined_train_pv_od$YEAR_MONTH_HOUR, format = "%Y-%m-%d %H")
combined_train_pv_od$seasonal_dummy <- as.integer(combined_train_pv_od$seasonal_dummy) - 1


combined_bus_pv_od <- rbind(pv_bus_od_202312, pv_bus_od_202401, pv_bus_od_202402) %>%
  mutate(trip = str_c(ORIGIN_PT_CODE, DESTINATION_PT_CODE, sep="-")) %>%
  #mutate(YEAR_MONTH = as.Date(paste(YEAR_MONTH,"01",sep = "-"))) %>% 
  mutate(YEAR_MONTH_HOUR = str_c(YEAR_MONTH, TIME_PER_HOUR, sep = "-")) %>%
  #mutate(YEAR_MONTH_HOUR = as.Date(ymd_h(YEAR_MONTH_HOUR))) %>%
  #mutate(YEAR_MONTH_HOUR = as.POSIXct(combined_train_pv_od$YEAR_MONTH_HOUR, format = "%Y-%m-%d %H")) %>%
  #mutate(trip = as.factor(trip)) %>%
  #mutate(seasonal_dummy = as.factor(seasonal_dummy)) %>%
  mutate(ORIGIN_PT_CODE = as.factor(ORIGIN_PT_CODE)) %>%
  mutate(DESTINATION_PT_CODE = as.factor(DESTINATION_PT_CODE))

#combined_bus_pv_od$YEAR_MONTH_HOUR <- as.POSIXct(combined_train_pv_od$YEAR_MONTH_HOUR, format = "%Y-%m-%d %H")

str(combined_bus_pv_od)

model <- keras_model_sequential()

n_timesteps <- 3 ## number of time steps in sequence / length of time series
n_features <- 3 ## number of predictors used in  model >> trips, seasonal_dummy, YEAR_MONTH_HOUR

model <- keras_model_sequential() %>% 
  layer_conv_1d(filters = 64, kernel_size = 3, activation = 'relu', input_shape = c(n_timesteps, n_features)) %>%
  layer_lstm(units = 50, activation = 'tanh', return_sequences = TRUE) %>% 
  layer_flatten() %>% 
  layer_dense(units = 1) %>% 
  compile(optimizer = 'adam', loss = 'mean_squared_error')

# Print the model summary
summary(model)

combined_train_pv_od$seasonal_dummy <- as.numeric(combined_train_pv_od$seasonal_dummy)

X_train <- combined_train_pv_od[, c("YEAR_MONTH_HOUR", "trip", "seasonal_dummy")] 
X_train <- array(data = X_train, dim = c(nrow(X_train), n_timesteps, n_features)) 
y_train <- combined_train_pv_od$TOTAL_TRIPS 

