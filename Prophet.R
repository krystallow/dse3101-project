
library(tidyverse)
library(lubridate)
library(prophet)

# combine data from Dec 2023 - Feb 2024 of passenger volume for trains with origin destinations
pv_train_od = rbind(pv_train_od_202312,pv_train_od_202401,pv_train_od_202402)

# change class of date to Date
pv_train_od = pv_train_od %>%
  mutate(YEAR_MONTH = as.Date(paste(YEAR_MONTH,"01",sep = "-")))

pv_train_od$ds = pv_train_od$YEAR_MONTH
# Combine date and hour
pv_train_od$ds <- as.POSIXct(paste(pv_train_od$ds, pv_train_od$TIME_PER_HOUR), format = "%Y-%m-%d %H")

#converting everything to numeric for Prophet
pv_train_od$y = pv_train_od$TOTAL_TRIPS
pv_train_od$ORIGIN_PT_CODE <- as.numeric(factor(pv_train_od$ORIGIN_PT_CODE))
pv_train_od$DESTINATION_PT_CODE <- as.numeric(factor(pv_train_od$DESTINATION_PT_CODE))
#pv_train_od$is_weekday <- ifelse(pv_train_od$DAY_TYPE == 'WEEKDAY', 1, 0)
pv_train_od$DAY_TYPE <- as.numeric(factor(pv_train_od$DAY_TYPE))


pv_train_od = pv_train_od %>%
  select(-PT_TYPE) %>%
  select(-TOTAL_TRIPS) %>%
  select(-YEAR_MONTH)


# Create dummy variables
#origin_destination_dummies <- model.matrix(~ ORIGIN_PT_CODE - 1, data = pv_train_od)
#final_destination_dummies <- model.matrix(~ DESTINATION_PT_CODE - 1, data = pv_train_od)

# Bind the dummy variables to your original dataframe
#pv_train_od <- cbind(pv_train_od, origin_destination_dummies, final_destination_dummies)

# Initialize model
m <- prophet()

# Add additional regressors
m <- add_regressor(m, 'DAY_TYPE')
m <- add_regressor(m, 'TIME_PER_HOUR')
m <- add_regressor(m, 'ORIGIN_PT_CODE')
m <- add_regressor(m, 'DESTINATION_PT_CODE')
# Fit the model
m <- fit.prophet(m, pv_train_od)

# Make future dataframe
#future <- make_future_dataframe(m, periods = 900000) 

# Add additional regressors to future dataframe
#USE MARCH FOR FUTURE DATAFRAME
#future$DAY_TYPE <- sample(min(pv_train_od$DAY_TYPE):max(pv_train_od$DAY_TYPE), 900020, replace = TRUE)
#future$TIME_PER_HOUR <- sample(min(pv_train_od$TIME_PER_HOUR):max(pv_train_od$TIME_PER_HOUR), 900020, replace = TRUE)
#future$ORIGIN_PT_CODE <- sample(min(pv_train_od$ORIGIN_PT_CODE):max(pv_train_od$ORIGIN_PT_CODE), 900020, replace = TRUE)
#future$DESTINATION_PT_CODE <- sample(min(pv_train_od$DESTINATION_PT_CODE):max(pv_train_od$DESTINATION_PT_CODE), 900020, replace = TRUE)

# Predict
#forecast <- predict(m, future)

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

