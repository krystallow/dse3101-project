
library(tidyverse)
library(stringr)
#install.packages("keras", "tensorflow")
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
  mutate(trip = as.factor(trip)) %>%
  mutate(seasonal_dummy = as.factor(seasonal_dummy)) %>%
  mutate(ORIGIN_PT_CODE = as.factor(ORIGIN_PT_CODE)) %>%
  mutate(DESTINATION_PT_CODE = as.factor(DESTINATION_PT_CODE)) %>%
  mutate(YEAR_MONTH = as.Date(paste(YEAR_MONTH,"01",sep = "-")))
  
combined_train_pv_od$YEAR_MONTH_HOUR = combined_train_pv_od$YEAR_MONTH
combined_train_pv_od$YEAR_MONTH_HOUR = as.POSIXct(paste(combined_train_pv_od$YEAR_MONTH_HOUR, combined_train_pv_od$TIME_PER_HOUR), format = "%Y-%m-%d %H") 

combined_train_pv_od$Year <- as.numeric(format(combined_train_pv_od$YEAR_MONTH_HOUR, "%Y"))
combined_train_pv_od$Month <- as.numeric(format(combined_train_pv_od$YEAR_MONTH_HOUR, "%m"))
combined_train_pv_od$Hour <- as.numeric(format(combined_train_pv_od$YEAR_MONTH_HOUR, "%H"))
combined_train_pv_od$trip_numeric <- as.numeric(as.factor(combined_train_pv_od$trip))
combined_train_pv_od$seasonal_dummy <- as.numeric(combined_train_pv_od$seasonal_dummy)

str(combined_train_pv_od)

# Prepare input features and target variable
X <- combined_train_pv_od[, c("Year", "Month", "Hour", "seasonal_dummy", "trip_numeric")]
y <- combined_train_pv_od$TOTAL_TRIPS

# Normalize features (optional but recommended)
X <- scale(X)
str(X)
str(y)

# Split data into training and validation sets
train_size <- 0.8
train_samples <- floor(train_size * nrow(X))
X_train <- X[1:train_samples, , drop = FALSE]
y_train <- y[1:train_samples]
X_val <- X[(train_samples + 1):nrow(X), , drop = FALSE]
y_val <- y[(train_samples + 1):nrow(X)]

# Create ConvLSTM model
model <- keras_model_sequential()
model %>%
  layer_conv_1d(filters = 32, kernel_size = 3, activation = "relu", input_shape = c(5, 1)) %>%
  #layer_flatten() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1)

# Compile the model
model %>%
  compile(optimizer = "adam", loss = "mean_squared_error")

summary(model)

# Train the model using training data
history <- model %>% fit(
  x = X_train,
  y = y_train,
  epochs = 50, #single pass through entire dataset during model training > determines how many times the model will see the entire dataset
  batch_size = 32, #number of data points processed tgt in each iteration > common sizes: 32,64,128
  validation_data = list(X_val, y_val),
  verbose = 1
)

# Evaluate the model (use test data if available)
val_loss <- history$metrics$val_loss[length(history$metrics$val_loss)]
cat("Validation Loss:", val_loss, "\n")

# Make predictions
y_pred <- model %>% predict(X_val)



