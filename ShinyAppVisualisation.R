#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


#packages needed
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(sf)
library(readr)
library(tmap)
library(plotly)

###################
# Loading in data #
###################

# Passenger Volume for Trains - By Train Stops (2023 Dec)
pv_train_202312 <- read.csv("../../data/transport_node_train_202312.csv")

# Passenger Volume for Trains - By Train Stops (2024 Feb)
pv_train_202402 <- read.csv("../../data/transport_node_train_202402.csv")

# Passenger Volume for Trains - By Origin Destination (2023 Dec)
pv_train_od_202312 <- read.csv("../../data/origin_destination_train_202312.csv")

# Passenger Volume for Trains - By Origin Destination (2024 Feb)
pv_train_od_202402 <- read.csv("../../data/origin_destination_train_202402.csv")

# Geography of Island
shape = st_make_valid(st_read("../../data/MasterPlan2019SubzoneBoundaryNoSeaGEOJSON.geojson"))

# Train Stations in Singapore (using data from Kaggle)
shape_train = read_csv("../../data/TrainStations.csv") %>%
  select(-geometry) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(shape))

# for forecast
forecast_lstm = read_excel("../../data/forecast_LSTM.xlsx")
popagsex <- read.csv("../../data/hsetod2000to2020/hsetod2000to2020.csv")
mpsz <- st_read(dsn = "../../data/MP14_SUBZONE_WEB_PL.shx")

#################
# data cleaning #
#################

#trainstn_in_region: merging shape_train and shape, to see which region each train stn belongs to
shape_train = st_transform(shape_train, st_crs(shape))
trainstn_in_region = st_join(shape_train, shape, left = F)

###pv_train, 202402
pv_train_feb = pv_train_202402 %>%
  select(-YEAR_MONTH, -TIME_PER_HOUR) %>%
  mutate(DAY_TYPE = as.factor(DAY_TYPE), VOLUME = TOTAL_TAP_IN_VOLUME + TOTAL_TAP_OUT_VOLUME) %>%
  group_by(PT_CODE) %>%
  summarise(TOTAL_VOLUME = sum(VOLUME)) %>%
  arrange(desc(TOTAL_VOLUME)) %>%
  mutate(PT_CODE = as.character(PT_CODE))

pv_train_feb_weekday = pv_train_202402 %>%
  select(-YEAR_MONTH, -TIME_PER_HOUR) %>%
  mutate(DAY_TYPE = as.factor(DAY_TYPE), VOLUME = TOTAL_TAP_IN_VOLUME + TOTAL_TAP_OUT_VOLUME) %>%
  filter(DAY_TYPE == 'WEEKDAY') %>%
  group_by(PT_CODE) %>%
  summarise(AVG_DAILY_VOLUME = sum(VOLUME)/20) %>% #Feb have 20 weekdays
  arrange(desc(AVG_DAILY_VOLUME)) %>%
  mutate(PT_CODE = as.character(PT_CODE))

pv_train_feb_weekend = pv_train_202402 %>%
  select(-YEAR_MONTH, -TIME_PER_HOUR) %>%
  mutate(DAY_TYPE = as.factor(DAY_TYPE), VOLUME = TOTAL_TAP_IN_VOLUME + TOTAL_TAP_OUT_VOLUME) %>%
  filter(DAY_TYPE == 'WEEKENDS/HOLIDAY') %>%
  group_by(PT_CODE) %>%
  summarise(AVG_DAILY_VOLUME = sum(VOLUME)/9) %>% #Feb have 9 weekends/hol
  arrange(desc(AVG_DAILY_VOLUME)) %>%
  mutate(PT_CODE = as.character(PT_CODE))

###pv_train, 202312
pv_train_dec = pv_train_202312 %>%
  select(-YEAR_MONTH, -TIME_PER_HOUR) %>%
  mutate(DAY_TYPE = as.factor(DAY_TYPE), VOLUME = TOTAL_TAP_IN_VOLUME + TOTAL_TAP_OUT_VOLUME) %>%
  group_by(PT_CODE) %>%
  summarise(TOTAL_VOLUME = sum(VOLUME)) %>%
  arrange(desc(TOTAL_VOLUME)) %>%
  mutate(PT_CODE = as.character(PT_CODE))

pv_train_dec_weekday = pv_train_202312 %>%
  select(-YEAR_MONTH, -TIME_PER_HOUR) %>%
  mutate(DAY_TYPE = as.factor(DAY_TYPE), VOLUME = TOTAL_TAP_IN_VOLUME + TOTAL_TAP_OUT_VOLUME) %>%
  filter(DAY_TYPE == 'WEEKDAY') %>%
  group_by(PT_CODE) %>%
  summarise(AVG_DAILY_VOLUME = sum(VOLUME)/20) %>% #Dec have 20 weekdays
  arrange(desc(AVG_DAILY_VOLUME)) %>%
  mutate(PT_CODE = as.character(PT_CODE))

pv_train_dec_weekend = pv_train_202312 %>%
  select(-YEAR_MONTH, -TIME_PER_HOUR) %>%
  mutate(DAY_TYPE = as.factor(DAY_TYPE), VOLUME = TOTAL_TAP_IN_VOLUME + TOTAL_TAP_OUT_VOLUME) %>%
  filter(DAY_TYPE == 'WEEKENDS/HOLIDAY') %>%
  group_by(PT_CODE) %>%
  summarise(AVG_DAILY_VOLUME = sum(VOLUME)/11) %>% #Dec have 11 weekends/hol
  arrange(desc(AVG_DAILY_VOLUME)) %>%
  mutate(PT_CODE = as.character(PT_CODE))


trainstn_in_region_dec = trainstn_in_region %>%
  left_join(pv_train_dec, by = c("STN_NO" = "PT_CODE")) %>%
  group_by(Name) %>%
  summarise(TOTAL_VOLUME = sum(TOTAL_VOLUME)) %>%
  select(Name, TOTAL_VOLUME)

trainstn_in_region_feb = trainstn_in_region %>%
  left_join(pv_train_feb, by = c("STN_NO" = "PT_CODE")) %>%
  group_by(Name) %>%
  summarise(TOTAL_VOLUME = sum(TOTAL_VOLUME)) %>%
  select(Name, TOTAL_VOLUME)

trainstn_in_region_wd_dec = trainstn_in_region %>%
  left_join(pv_train_dec_weekday, by = c("STN_NO" = "PT_CODE")) %>%
  group_by(Name) %>%
  summarise(AVG_DAILY_VOLUME = sum(AVG_DAILY_VOLUME)) %>%
  select(Name, AVG_DAILY_VOLUME) %>%
  arrange(desc(AVG_DAILY_VOLUME)) %>%
  head(10) %>%
  mutate(day_type = "weekday")

trainstn_in_region_we_dec = trainstn_in_region %>%
  left_join(pv_train_dec_weekend, by = c("STN_NO" = "PT_CODE")) %>%
  group_by(Name) %>%
  summarise(AVG_DAILY_VOLUME = sum(AVG_DAILY_VOLUME)) %>%
  select(Name, AVG_DAILY_VOLUME) %>%
  arrange(desc(AVG_DAILY_VOLUME)) %>%
  head(10) %>%
  mutate(day_type = "weekend/holiday")

trainstn_in_region_wd_feb = trainstn_in_region %>%
  left_join(pv_train_feb_weekday, by = c("STN_NO" = "PT_CODE")) %>%
  group_by(Name) %>%
  summarise(AVG_DAILY_VOLUME = sum(AVG_DAILY_VOLUME)) %>%
  select(Name, AVG_DAILY_VOLUME) %>%
  arrange(desc(AVG_DAILY_VOLUME)) %>%
  head(10) %>%
  mutate(day_type = "weekday")

trainstn_in_region_we_feb = trainstn_in_region %>%
  left_join(pv_train_feb_weekend, by = c("STN_NO" = "PT_CODE")) %>%
  group_by(Name) %>%
  summarise(AVG_DAILY_VOLUME = sum(AVG_DAILY_VOLUME)) %>%
  select(Name, AVG_DAILY_VOLUME) %>%
  arrange(desc(AVG_DAILY_VOLUME)) %>%
  head(10) %>%
  mutate(day_type = "weekend/holiday")



##############################
#forecast_lstm data cleaning #
##############################

# forecast using LSTM
day_type <- ifelse(forecast_lstm$DAY_TYPE_WEEKENDS.HOLIDAY == 1,
                   "Weekend/Holiday",
                   ifelse(forecast_lstm$DAY_TYPE_WEEKDAY == 1,
                          "Weekday",
                          "NA"))
forecast_lstm = forecast_lstm %>%
  mutate(day_type = day_type) %>%
  select(-DAY_TYPE_WEEKENDS.HOLIDAY, -DAY_TYPE_WEEKDAY)



### making station variable ###

# function to pass data through to make it categorical
make_categorical <- function(data) {
  categorical_column <- nrow(data)
  for (i in 1:nrow(data)) {
    if (data[i, which(data[i, ] == 1)] == 1) {
      categorical_column[i] <- colnames(data)[which(data[i, ] == 1)]
    }
  }
  return(categorical_column)
}

# only DEST_PT_CODE columns
forecast_lstm_deststns = forecast_lstm %>%
  select(starts_with("DESTINATION"))
colnames(forecast_lstm_deststns) = str_remove(colnames(forecast_lstm_deststns), "DESTINATION_PT_CODE_")

deststns_col = make_categorical(forecast_lstm_deststns)

# only ORIGIN_PT_CODE columns
forecast_lstm_originstns = forecast_lstm %>%
  select(starts_with("ORIGIN"))
colnames(forecast_lstm_originstns) = str_remove(colnames(forecast_lstm_originstns), "ORIGIN_PT_CODE_")

originstns_col = make_categorical(forecast_lstm_originstns)


### Making cleaned dataset with no indicator variables ###

forecast_lstm_clean = forecast_lstm %>%
  select(Predicted_TOTAL_TRIPS, TIME_PER_HOUR, day_type) %>%
  mutate(Origin = originstns_col, Destination = deststns_col, Forecasted = Predicted_TOTAL_TRIPS) 

forecast_lstm_clean$Origin = gsub("\\.", "/", forecast_lstm_clean$Origin)
forecast_lstm_clean$Destination = gsub("\\.", "/", forecast_lstm_clean$Destination)

forecast_lstm_clean_wd = forecast_lstm_clean %>%
  filter(day_type == "Weekday")
forecast_lstm_clean_weh = forecast_lstm_clean %>%
  filter(day_type == "Weekend/Holiday")

##### Code to get forecasted passenger volume (PV), at individual train stations #####

trainstn_in_region_2 = trainstn_in_region

forecast_origin_wd = forecast_lstm_clean_wd %>%
  group_by(Origin) %>%
  summarise(forecasted_tripsfrom = sum(Forecasted)) #getting PV at train stns, using origin stn (people FROM these stations)
forecast_dest_wd = forecast_lstm_clean_wd %>%
  group_by(Destination) %>%
  summarise(forecasted_tripsto = sum(Forecasted)) #getting PV at train stns, using destination stn (people going TO these stations)

forecast_origin_weh = forecast_lstm_clean_weh %>%
  group_by(Origin) %>%
  summarise(forecasted_tripsfrom = sum(Forecasted)) #getting PV at train stns, using origin stn (people FROM these stations)
forecast_dest_weh = forecast_lstm_clean_weh %>%
  group_by(Destination) %>%
  summarise(forecasted_tripsto = sum(Forecasted)) #getting PV at train stns, using destination stn (people going TO these stations)

# cleaning up `trainstn_in_region` dataframe, to be able to merge this with the `forecast` dataframes (station names do not tally, so this part makes the station names the same)
trainstn_in_region_2$STN_NO = sub("NS4", "NS4/BP1", trainstn_in_region_2$STN_NO)
trainstn_in_region_2$STN_NO = sub("DT1", "BP6/DT1", trainstn_in_region_2$STN_NO)
trainstn_in_region_2$STN_NO = sub("EW13/NS25", "NS25/EW13", trainstn_in_region_2$STN_NO)
trainstn_in_region_2$STN_NO = sub("NE17", "NE17/PTC", trainstn_in_region_2$STN_NO)
trainstn_in_region_2$STN_NO = sub("NS22/TE14", "TE14/NS22", trainstn_in_region_2$STN_NO)
trainstn_in_region_2$STN_NO = sub("DT16/CE1", "CE1/DT16", trainstn_in_region_2$STN_NO)
trainstn_in_region_2$STN_NO = sub("NE16", "NE16/STC", trainstn_in_region_2$STN_NO)
trainstn_in_region_2$STN_NO = sub("NS27/TE20/CE2", "NS27/CE2/TE20", trainstn_in_region_2$STN_NO)
trainstn_in_region_2$STN_NO = sub("EW16", "EW16/NE3/TE17", trainstn_in_region_2$STN_NO)
trainstn_in_region_2$STN_NO = sub("CC4/BP6/DT15", "CC4/DT15", trainstn_in_region_2$STN_NO)
trainstn_in_region_2$STN_NO = sub("BP6/CE1/DT16", "CE1/DT16", trainstn_in_region_2$STN_NO)
trainstn_in_region_2$STN_NO = sub("BP6/DT10/TE11", "DT10/TE11", trainstn_in_region_2$STN_NO)
trainstn_in_region_2$STN_NO = sub("BP6/DT13", "DT13", trainstn_in_region_2$STN_NO)
trainstn_in_region_2$STN_NO = sub("BP6/DT13", "DT13", trainstn_in_region_2$STN_NO)
trainstn_in_region_2$STN_NO = sub("BP6/DT17", "DT17", trainstn_in_region_2$STN_NO)
trainstn_in_region_2$STN_NO = sub("BP6/DT18", "DT18", trainstn_in_region_2$STN_NO)
trainstn_in_region_2$STN_NO = sub("EW12/BP6/DT14", "EW12/DT14", trainstn_in_region_2$STN_NO)
trainstn_in_region_2$STN_NO = sub("NE4/BP6/DT19", "NE4/DT19", trainstn_in_region_2$STN_NO)
trainstn_in_region_2$STN_NO = sub("NE7/BP6/DT12", "NE7/DT12", trainstn_in_region_2$STN_NO)
trainstn_in_region_2$STN_NO = sub("NS21/BP6/DT11", "NS21/DT11", trainstn_in_region_2$STN_NO)


forecast_totalPV_wd = inner_join(forecast_origin_wd, forecast_dest_wd, by = c("Origin" = "Destination")) %>%
  mutate(forecastedPV = forecasted_tripsfrom + forecasted_tripsto, station = Origin) %>%
  select(station, forecastedPV) %>%
  full_join(trainstn_in_region_2, by = c("station" = "STN_NO")) %>%
  select(station, STN_NAME, forecastedPV, geometry, Name) %>%
  na.omit() %>% #omitting the train stations we do not have the location for
  mutate(Region_code = Name, day_type = "Weekday")
forecast_totalPV_weh = inner_join(forecast_origin_weh, forecast_dest_weh, by = c("Origin" = "Destination")) %>%
  mutate(forecastedPV = forecasted_tripsfrom + forecasted_tripsto, station = Origin) %>%
  select(station, forecastedPV) %>%
  full_join(trainstn_in_region_2, by = c("station" = "STN_NO")) %>%
  select(station, STN_NAME, forecastedPV, geometry, Name) %>%
  na.omit() %>% #omitting the train stations we do not have the location for
  mutate(Region_code = Name, day_type = "Weekend/Holiday")

forecast_totalPV = rbind(forecast_totalPV_wd, forecast_totalPV_weh)
### final dataframe `forecast_totalPV` contains information about each station and its geometric location (geometry point), and it's forecasted PV.


#Filtering and Getting the Total Population
mpsz_popagsex2019_total <- popagsex %>%
  filter(Time == 2019) %>%
  group_by(SZ) %>%
  summarise(Total_Population = sum(Hse))

#Joining 2019 Population and Planning Subzone dataframe
mpsz$SUBZONE_N <- str_to_title(mpsz$SUBZONE_N)

mpsz_popagsex2019 <- left_join(mpsz, mpsz_popagsex2019_total,
                               by = c("SUBZONE_N" = "SZ")) %>%
  select(3, 6, 8, 16)

validity <- sf::st_is_valid(mpsz_popagsex2019)
if (!all(validity)) {
  # If there are invalid geometries, attempt to fix them
  mpsz_popagsex2019 <- sf::st_make_valid(mpsz_popagsex2019)
}








#############
# Shiny app #
#############

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Analysis & Forecasts of Singapore's Public Transport System"),
    tabsetPanel(
      tabPanel("Monthly Forecast of Passenger Volumes at Train Stations in Singapore, by day type", 
               sidebarLayout(
                 sidebarPanel = sidebarPanel(
                   selectInput("year", ##follow output!! for choosing year
                               "Select year:",
                               choices = 2024),
                   selectInput("Month", "Select month:", choices = c("Feb")),
                   selectInput("day_type", "Select day type:", choices = c("Weekday", "Weekend/Holiday"))
                 ), ##follow output for choosing month & day_type! and choices must follow month names & day_type input.
                 
                 # Show a plot of the generated distribution
                 mainPanel = mainPanel(
                   tmapOutput("forecast")
                 )
               )
      ),
      tabPanel("Exploration & Analysis of data",
               tabsetPanel(
                 tabPanel("Comparing Weekday & Weekend/holiday Passenger Volumes at Train Stations", 
                         sidebarLayout(
                           sidebarPanel = sidebarPanel(
                               selectInput("month", "Select month:", choices = c("Feb", "Dec"))
                           ),
                           mainPanel = mainPanel(
                             plotlyOutput('wdvswe')
                           )
                         )
                  ),
                 tabPanel("Weekday vs Weekend/Holiday Passenger Volumes at Train Stations by Time of Day",
                          sidebarLayout(
                            sidebarPanel = sidebarPanel(
                              selectInput("month", "Select month:", choices = c("Feb", "Dec"))
                            ),
                            mainPanel = mainPanel(
                              plotOutput('timeseries')
                            )
                          )
                  )
              )
      )
    )
)





server <- function(input, output) {

    output$forecast <- renderTmap({
      forecast_data = forecast_totalPV %>%
        filter(day_type == input$day_type)
      
      forecast_data <- separate(forecast_data, geometry, into = c("longitude", "latitude"), sep = " ")
      
      # Convert the columns to numeric
      forecast_data$longitude <- gsub("[^0-9.-]", "", forecast_data$longitude)
      forecast_data$latitude <- gsub("[^0-9.-]", "", forecast_data$latitude)
      
      # Create the sf object
      forecast_data_sf <- st_as_sf(forecast_data, coords = c("longitude", "latitude"))
      
      # Set the CRS if necessary
      st_crs(forecast_data_sf) <- "+proj=longlat +datum=WGS84"
      
      #plot map
      bubble_size <- 0.5
      
      # Plot map
      custom_breaks <- c(0, 9999, 19999, 29999, 39999, 49999)
      
      tmap_mode("view")
      tm_shape(mpsz_popagsex2019) +
        tm_fill("Total_Population",
                title = "Total Population",
                n = 5,
                palette = "Purples",
                breaks = custom_breaks,
                popup.vars = c("REGION_N", "Total_Population")) +
        tm_borders(alpha = 0.5) +
        tm_shape(forecast_data_sf) +
        tm_bubbles(size = bubble_size, 
                   title.col = "Average Tap In & Tap Out Volume",
                   col = "forecastedPV",
                   palette = "Greens",
                   border.col = "black",
                   border.lwd = 1)
    })
    output$wdvswe = renderPlotly({
      trainstn_in_region_wdVSwe_dec = rbind(trainstn_in_region_wd_dec, trainstn_in_region_we_dec) %>%
        mutate(month = "Feb")
      trainstn_in_region_wdVSwe_feb = rbind(trainstn_in_region_wd_feb, trainstn_in_region_we_feb) %>%
        mutate(month = "Dec")
      data = rbind(trainstn_in_region_wdVSwe_dec, trainstn_in_region_wdVSwe_feb)
      data = filter(data, month == input$month)
      p6 = data %>%
        ggplot(aes(x=Name ,y=AVG_DAILY_VOLUME)) +
          geom_line(aes(group=Name), color = 'grey') +
          geom_point(aes(color = day_type), alpha = 0.7) +
          labs(title = "Total trips to each region, weekday vs weekend", y="No. of trips to", x = "Region code") +
          theme_minimal()
      p6 = ggplotly(p6)
      p6
    })
    output$timeseries = renderPlot({
      pv_traindec = mutate(pv_train_od_202312, month = "Dec")
      pv_trainfeb = mutate(pv_train_od_202402, month = "Feb")
      pv_train_od = rbind(pv_traindec, pv_trainfeb)
      pv_train_od <- 
        pv_train_od %>%
        filter(month == input$month) %>%
        mutate(Location = ifelse(ORIGIN_PT_CODE < DESTINATION_PT_CODE, ORIGIN_PT_CODE, DESTINATION_PT_CODE))
      
      # Grouping data by TIME_PER_HOUR and location, and summing up the total trips
      grouped_data <- pv_train_od %>%
        group_by(TIME_PER_HOUR, DAY_TYPE, Location) %>%
        summarise(Total_Trips = sum(TOTAL_TRIPS))
      
      # Sorting the total trips per location in descending order and selecting top 20 locations
      top_20_locations <- grouped_data %>%
        group_by(Location) %>%
        summarise(Total_Trips = sum(Total_Trips)) %>%
        top_n(20, Total_Trips)
      
      # Filter the top 20 locations
      grouped_data <- grouped_data %>%
        filter(Location %in% top_20_locations$Location)
      
      # Plotting the time series graph
      ggplot(grouped_data, aes(x = TIME_PER_HOUR, y = Total_Trips, color = as.factor(Location))) +
        geom_line() +
        labs(title = "Total Trips vs. Time for Top 20 Locations",
             x = "Time (Hour)",
             y = "Total Trips",
             color = "Location") +
        facet_wrap(~DAY_TYPE) +  # Adding facet_wrap for weekdays and weekends
        theme_minimal() +
        theme(legend.position = "top")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)









