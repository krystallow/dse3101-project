library(tidyverse)
library(stringr)
library(keras)

# Passenger Volume for Trains - By Origin Destination (2023 Dec)
pv_train_202312 <- read.csv("../PV Data [Buses and Trains]/Trains/By OD/origin_destination_train_202312.csv")

# Passenger Volume for Trains - By Train Stops (2024 Jan)
pv_train_202401 <- read.csv("../PV Data [Buses and Trains]/Trains/By OD/origin_destination_train_202401.csv")

# Passenger Volume for Trains - By Train Stops (2024 Feb)
pv_train_202402 <- read.csv("../PV Data [Buses and Trains]/Trains/By OD/origin_destination_train_202402.csv")

train_combined <- rbind(pv_train_202312, pv_train_202401, pv_train_02402)

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
# Apply the same for other categorical columns 
train_combined_encoded <- encode_categorical(train_combined_encoded, "DESTINATION_PT_CODE")
train_combined_encoded <- encode_categorical(train_combined_encoded, "ORIGIN_PT_CODE")

# Now, train_combined_encoded has one-hot encoded variables for the specified column
# check the first few rows to see the transformation
head(train_combined_encoded)


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


model <- keras_model_sequential() %>%
  layer_lstm(units = 50, return_sequences = TRUE, input_shape = c(1, ncol(X_train_subset))) %>%
  layer_lstm(units = 30, return_sequences = FALSE) %>% # Additional LSTM layer
  layer_dense(units = 20, activation = 'relu') %>% # An additional dense layer for complexity
  layer_dense(units = 1) # Output layer


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
# loss: 66638.53125 mean_absolute_error:  93.09532 

y_pred <- model %>% predict(X_test_array)   

predictions <- model %>% predict(X_test_array)

predictions_vector <- as.vector(predictions)

summary(predictions_vector)

# Create a data frame for comparing actual vs. predicted values
forecast_df <- data.frame(True_Values = y_test_subset, 
                            Predicted_Values = predictions_vector,
                            X_test_subset)

plot(x = forecast_df$Predicted_Values, 
     y = forecast_df$True_Values, 
     xlab = "Predicted Values", 
     ylab = "True Values",
     main = "Predicted vs. Actual Values",
     col = "blue")

# Add a diagonal line for reference (perfect prediction)
abline(a = 0, b = 1, col = "red", lwd=3)


library(Metrics)
RMSFE <- sqrt(mean_squared_residuals) #259.87 
cat("Root Mean Squared Forecast Error (RMSFE):", round(RMSFE, 2), "\n")

forecasted_data <- data.frame(   
  Predicted_TOTAL_TRIPS = y_pred,   
  X_test_subset )  

library(openxlsx)
write.xlsx(forecasted_data, file = "../forecast_LSTM.xlsx", rowNames = FALSE)






