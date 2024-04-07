library(tidyverse)
library(stringr)
library(keras)
# Passenger Volume for Trains - By Train Stops (2024 Jan)
pv_train_202401 <- read.csv("../PV Data [Buses and Trains]/Trains/By OD/origin_destination_train_202401.csv")

# Passenger Volume for Trains - By Train Stops (2024 Feb)
test_202402 <- read.csv("../PV Data [Buses and Trains]/Trains/By OD/origin_destination_train_202402.csv")


# Passenger Volume for Trains - By Origin Destination (2023 Dec)
pv_train_202312 <- read.csv("../PV Data [Buses and Trains]/Trains/By OD/origin_destination_train_202312.csv")

train_combined <- rbind(pv_train_202312, pv_train_202401, test_202402)

# Assuming the first day of each month for the YEAR_MONTH column
train_combined$YEAR_MONTH <- as.Date(paste0(train_combined$YEAR_MONTH, "-01"))

summary(train_combined$TOTAL_TRIPS)
table(train_combined$DAY_TYPE)
hist(train_combined$TOTAL_TRIPS, breaks=50, main="Distribution of Total Trips")


boxplot(TOTAL_TRIPS ~ DAY_TYPE, data=train_combined, main="Total Trips by Day Type")

# Function to manually perform one-hot encoding for a single categorical variable
encode_categorical <- function(df, col_name) {
  # Get unique categories for the specified column
  categories <- unique(df[[col_name]])
  
  # Create a binary column for each category
  for(category in categories) {
    df[[paste(col_name, category, sep = "_")]] <- if_else(df[[col_name]] == category, 1, 0)
  }
  
  return(df)
}

# Example usage for "DAY_TYPE" column
train_combined_encoded <- encode_categorical(train_combined, "DAY_TYPE")
# Apply the same for other categorical columns as needed
train_combined_encoded <- encode_categorical(train_combined_encoded, "DESTINATION_PT_CODE")
train_combined_encoded <- encode_categorical(train_combined_encoded, "ORIGIN_PT_CODE")

# Now, train_combined_encoded has one-hot encoded variables for the specified column
# You can check the first few rows to see the transformation
head(train_combined_encoded)


# Assuming train_combined_encoded and test_202402_encoded are your current datasets

# Drop the specified columns from the training dataset
train_combined_encoded <- train_combined_encoded %>%
  select(-DESTINATION_PT_CODE, -ORIGIN_PT_CODE, -PT_TYPE, -DAY_TYPE, -YEAR_MONTH)


train_combined_encoded

# Selecting the first 10,000 rows for both features and target variable
subset_data <- train_combined_encoded[1:10000, ]
X_subset <- as.matrix(subset_data[, setdiff(names(subset_data), "TOTAL_TRIPS")])
y_subset <- subset_data[["TOTAL_TRIPS"]]

# Reshape input to be 3D [samples, time steps, features] for LSTM
X_subset_array <- array(X_subset, dim = c(nrow(X_subset), 1, ncol(X_subset)))


# Determine the split index
split_index <- 8500

# Split the features and target into training and testing sets
X_train_subset <- X_subset[1:split_index, ]
y_train_subset <- y_subset[1:split_index]

X_test_subset <- X_subset[(split_index + 1):10000, ]
y_test_subset <- y_subset[(split_index + 1):10000]





# Reshape input to be 3D [samples, time steps, features] for LSTM
X_train_array <- array(X_train_subset, dim = c(nrow(X_train_subset), 1, ncol(X_train_subset)))
X_test_array <- array(X_test_subset, dim = c(nrow(X_test_subset), 1, ncol(X_test_subset)))

#install.packages("tensorflow")
#library(tensorflow)
#tensorflow::install_tensorflow()

#install.packages("keras")
#keras::install_keras()
#library(keras)
#devtools::install_github("rstudio/reticulate")


#library(reticulate)
#use_python("/Users/elizaong/pythonenviron/bin/python", required = TRUE)
#Sys.setenv(RETICULATE_PYTHON = "/Users/elizaong/pythonenviron/bin/python")
library(keras)

# Define the LSTM model
model <- keras_model_sequential() %>%
  layer_lstm(units = 50, input_shape = c(1, ncol(X_train_subset))) %>%
  layer_dense(units = 1)

# Compile the model
model %>% compile(
  optimizer = 'adam',
  loss = 'mean_squared_error',
  metrics = c('mean_absolute_error')
)

# Train the model
history <- model %>% fit(
  X_train_array, y_train_subset,
  epochs = 10,
  batch_size = 128,
  validation_split = 0.2,
  verbose = 1
)




# Plotting training & validation loss values
plot(history)

# Or for more detailed plotting:
library(ggplot2)

# Extracting loss values
train_loss <- history$metrics$loss
val_loss <- history$metrics$val_loss
epochs <- 1:length(train_loss)

# Creating a data frame for ggplot
loss_data <- data.frame(Epoch = epochs, Training_Loss = train_loss, Validation_Loss = val_loss)

# Melting for ggplot aesthetics
loss_data_long <- reshape2::melt(loss_data, id.vars = "Epoch")

# Plot
ggplot(loss_data_long, aes(x = Epoch, y = value, color = variable)) +
  geom_line() +
  labs(title = "Training vs Validation Loss",
       y = "Loss",
       color = "Legend") +
  theme_minimal()



# Evaluate the model on the test set
model %>% evaluate(X_test_array, y_test_subset, verbose = 1)
# loss: 70990.265625 mean_absolute_error: 82.4373092651367

y_pred <- model %>% predict(X_test_array)   
RMSE <- sqrt(mean((y_test_subset - y_pred)^2)) 
cat("Root Mean Squared Error (RMSE):", round(RMSE, 2), "\n")  

forecasted_data <- data.frame(   
  Predicted_TOTAL_TRIPS = y_pred,   
  X_test_subset )  

write.csv(forecasted_data, file = "../forecast_LSTM.xlsx", row.names = FALSE)



##:::KRYSTAL TESTING :::##
training_set <- rbind(pv_train_202312, pv_train_202401)
training_set$YEAR_MONTH <- as.Date(paste0(training_set$YEAR_MONTH, "-01"))
training_set_encoded <- encode_categorical(training_set, "DAY_TYPE")
training_set_encoded <- encode_categorical(training_set_encoded, "DESTINATION_PT_CODE")
training_set_encoded <- encode_categorical(training_set_encoded, "ORIGIN_PT_CODE")

training_set_encoded <- training_set_encoded %>%
  select(-DESTINATION_PT_CODE, -ORIGIN_PT_CODE, -PT_TYPE, -DAY_TYPE, -YEAR_MONTH)

X <- as.matrix(training_set_encoded[, setdiff(names(training_set_encoded), "TOTAL_TRIPS")])
y <- training_set_encoded[["TOTAL_TRIPS"]]

X_array <- array(X, dim = c(nrow(X), 1, ncol(X)))

test_set <- test_202402
test_set$YEAR_MONTH <- as.Date(paste0(test_set$YEAR_MONTH, "-01"))
test_set_encoded <- encode_categorical(test_set, "DAY_TYPE")
test_set_encoded <- encode_categorical(test_set_encoded, "DESTINATION_PT_CODE")
test_set_encoded <- encode_categorical(test_set_encoded, "ORIGIN_PT_CODE")

test_set_encoded <- test_set_encoded %>%
  select(-DESTINATION_PT_CODE, -ORIGIN_PT_CODE, -PT_TYPE, -DAY_TYPE, -YEAR_MONTH)

X_test <- as.matrix(test_set_encoded[, setdiff(names(test_set_encoded), "TOTAL_TRIPS")])
y_test <- test_set_encoded[["TOTAL_TRIPS"]]

X_test_array <- array(X, dim = c(nrow(X), 1, ncol(X)))

model <- keras_model_sequential() %>%
  layer_lstm(units = 50, input_shape = c(1, ncol(X))) %>%
  layer_dense(units = 1)

# Compile the model
model %>% compile(
  optimizer = 'adam',
  loss = 'mean_squared_error',
  metrics = c('mean_absolute_error')
)

# Train the model
history <- model %>% fit(
  X_array, y,
  epochs = 10, ## KEEP INCREASING AS LONG AS VAL LOSS IS DECREASING >> try 20 next
  batch_size = 128,
  validation_split = 0.2,
  verbose = 1
)


model %>% evaluate(X_test_array, y_test, verbose = 1)


y_pred <- model %>% predict(X_test_array)   
RMSE <- sqrt(mean((y_test - y_pred)^2)) 
cat("Root Mean Squared Error (RMSE):", round(RMSE, 2), "\n")

##:::KRYSTAL TESTING :::## 








############################################
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


# Create LSTM model (TRY THIS - ELIZA!!!!!!)
model <- keras_model_sequential()
model %>%
  layer_lstm(units = 50, return_sequences = TRUE, input_shape = c(5, 1)) %>%
  layer_lstm(units = 50) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1)

# Compile the model
model %>%
  compile(optimizer = "adam", loss = "mean_squared_error")

summary(model)

# Train the model using training data
history <- model %>% fit(
  x = array_reshape(X_train, c(dim(X_train)[1], dim(X_train)[2], 1)),
  y = y_train,
  epochs = 50,
  batch_size =32,
  validation_data=list(array_reshape(X_val,c(dim(X_val)[1],dim(X_val)[2],1)),y_val),
  verbose=1
)

## UNTIL HERE -ELIZA!!




# Check the shape of X_train
print(dim(X_train))  # Should match (batch_size, timesteps, rows, columns)

# Check the shape of X_val
print(dim(X_val))    # Should also match (batch_size, timesteps, rows, columns)

print(dim(y_train))


# Create ConvLSTM model
model <- keras_model_sequential()
model %>%
  layer_conv_1d(filters = 32, kernel_size = 3, activation = "relu", input_shape = c(5, 1)) %>%
  layer_flatten() %>%
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




######### USING 2D

X <- scale(X)

X <- array(X, dim = c(n_samples, n_timesteps, n_features, 1))  # Add the channel dimension

# Define the ConvLSTM model
model <- keras_model_sequential()
model %>%
  layer_conv_lstm_2d(filters = 32, kernel_size = 3, input_shape = c(5, 1, 1)) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1)  # Output layer for regression

# Compile the model
model %>% compile(
  optimizer = optimizer_adam(),
  loss = "mean_squared_error"
)

# Train the model
model %>% fit(X, y, epochs = 10, batch_size = 32)

# Make predictions
predictions <- model %>% predict(X)

#########
