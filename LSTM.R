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
## 8500: eval(model) loss: 70990.265625 mean_absolute_error: 82.4373092651367
### train_combined_encoded[1:10000, ]
### train_subset 1:8500, test_subset: (split_index + 1):10000


# Split the features and target into training and testing sets
X_train_subset <- X_subset[1:split_index, ]
y_train_subset <- y_subset[1:split_index]

X_test_subset <- X_subset[(split_index + 1):10000, ]
y_test_subset <- y_subset[(split_index + 1):10000]



# Reshape input to be 3D [samples, time steps, features] for LSTM
X_train_array <- array(X_train_subset, dim = c(nrow(X_train_subset), 1, ncol(X_train_subset)))
X_test_array <- array(X_test_subset, dim = c(nrow(X_test_subset), 1, ncol(X_test_subset)))


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

library(Metrics)
RMSE <- rmse(y_test_subset, y_pred)
cat("Root Mean Squared Error (RMSE):", round(RMSE, 2), "\n")

residuals <- y_test_subset - y_pred

squared_residuals <- residuals^2

mean_squared_residuals <- mean(squared_residuals)

RMSFE <- sqrt(mean_squared_residuals)
cat("Root Mean Squared Forecast Error (RMSFE):", round(RMSFE, 2), "\n")


forecasted_data <- data.frame(   
  Predicted_TOTAL_TRIPS = y_pred,   
  X_test_subset )  

library(openxlsx)
write.xlsx(forecasted_data, file = "../forecast_LSTM.xlsx", rowNames = FALSE)

# Plot ROC Curve
library(pROC)

roc_obj <- roc(y_test_subset, y_pred)

plot(roc_obj, col = "blue", lwd = 2, main = "ROC Curve")

# Add diagonal line representing random classifier
lines(x = c(0, 1), y = c(0, 1), col = "red", lty = 2)

# Calculate AUC
auc <- roc_obj$auc

# Print AUC value
text(0.7, 0.2, paste("AUC = ", round(auc, 2)), col = "blue", cex = 1.2)



##:::KRYSTAL TESTING :::##

### if we use dec2023 and jan2024 to train, feb2024 to test
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

X_test_array <- array(X_test, dim = c(nrow(X_test), 1, ncol(X_test)))

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
  epochs = 20, 
  batch_size = 128,
  validation_split = 0.2,
  verbose = 1
)


model %>% evaluate(X_test_array, 
                   y_test, 
                   verbose = 1)


y_pred <- model %>% predict(X_test_array)   
RMSE <- sqrt(mean((y_test - y_pred)^2)) 
cat("Root Mean Squared Error (RMSE):", round(RMSE, 2), "\n")
# Root Mean Squared Error (RMSE): 416.61 << worse, overfitting

##:::KRYSTAL TESTING :::## 



