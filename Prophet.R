library(tidyverse)
library(lubridate)
library(prophet)

# combine data from Dec 2023 - Feb 2024 of passenger volume for trains with origin destinations
pv_train_od = pv_train_od_202312

# change class of date to Date
pv_train_od = pv_train_od %>%
  mutate(YEAR_MONTH = as.Date(paste(YEAR_MONTH,"01",sep = "-")))

pv_train_od$ds = pv_train_od$YEAR_MONTH
# Combine date and hour
pv_train_od$ds <- as.POSIXct(paste(pv_train_od$ds, pv_train_od$TIME_PER_HOUR), format = "%Y-%m-%d %H")

pv_train_od$y = pv_train_od$TOTAL_TRIPS

pv_train_od$is_weekday <- ifelse(pv_train_od$DAY_TYPE == 'WEEKDAY', 1, 0)

# Convert 'origin_pt_code' and 'destination_pt_code' to factors
pv_train_od$ORIGIN_PT_CODE <- as.factor(pv_train_od$ORIGIN_PT_CODE)
pv_train_od$DESTINATION_PT_CODE <- as.factor(pv_train_od$DESTINATION_PT_CODE)

# Create dummy variables
origin_destination_dummies <- model.matrix(~ ORIGIN_PT_CODE - 1, data = pv_train_od)
final_destination_dummies <- model.matrix(~ DESTINATION_PT_CODE - 1, data = pv_train_od)

# Bind the dummy variables to your original dataframe
pv_train_od <- cbind(pv_train_od, origin_destination_dummies, final_destination_dummies)

# Initialize the model
model <- prophet(weekly.seasonality = FALSE)

# Add custom weekly seasonality
model <- add_seasonality(model, name='weekday', period=7, fourier.order=3, condition.name='is_weekday')

# Make valid names for the column names
#names(pv_train_od) <- make.names(names(pv_train_od), unique = TRUE)
#names(origin_destination_dummies) <- make.names(names(origin_destination_dummies), unique = TRUE)
#names(final_destination_dummies) <- make.names(names(final_destination_dummies), unique = TRUE)



# Now you can add the dummy variables as extra regressors
dummy_cols <- colnames(origin_destination_dummies)
for (col in dummy_cols) {
  model <- add_regressor(model, col)
}


# Fit the model
model <- fit.prophet(model, pv_train_od)

future <- make_future_dataframe(model, periods = 365)
future$is_weekday <- ifelse(future$DAY_TYPE == 'WEEKDAY', 1, 0)
forecast = predict(model,future)