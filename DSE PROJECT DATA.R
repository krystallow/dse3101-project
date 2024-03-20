library(httr)
library(jsonlite)
library(tidyverse)



# Bus Services Data
## Returns detailed service information for all buses currently in operation, including: first stop, last stop, peak / offpeak frequency of dispatch.

resource_url <- "http://datamall2.mytransport.sg/ltaodataservice/BusServices"


all_data <- list()
skip_value <- 0

while (TRUE) {
  res <- GET(paste0(resource_url, "?$skip=", skip_value), 
             add_headers(AccountKey = Sys.getenv("LTA_KEY"), accept = "application/json"))
  
  if (res$status_code == 200) {
    res_text <- content(res, as = "text")
    res_list <- fromJSON(res_text, flatten = TRUE)
    
    if (length(res_list$value) == 0) {
      break  # Exit loop when there are no more observations
    }
    
    all_data[[length(all_data) + 1]] <- res_list$value
    skip_value <- skip_value + 500
  } else {
    stop("Failed to retrieve data. Status code: ", res$status_code)
  }
}

bus_services <- bind_rows(all_data)


# Bus Routes
## Returns detailed route information for all services currently in operation, including: all bus stops along each route, first/last bus timings for each stop.

resource_url <- "http://datamall2.mytransport.sg/ltaodataservice/BusRoutes"
all_data <- list()
skip_value <- 0

while (TRUE) {
  res <- GET(paste0(resource_url, "?$skip=", skip_value), 
             add_headers(AccountKey = Sys.getenv("LTA_KEY"), accept = "application/json"))
  
  if (res$status_code == 200) {
    res_text <- content(res, as = "text")
    res_list <- fromJSON(res_text, flatten = TRUE)
    
    if (length(res_list$value) == 0) {
      break  # Exit loop when there are no more observations
    }
    
    all_data[[length(all_data) + 1]] <- res_list$value
    skip_value <- skip_value + 500
  } else {
    stop("Failed to retrieve data. Status code: ", res$status_code)
  }
}

bus_routes <- bind_rows(all_data)


# Bus Stops
## Returns detailed information for all bus stops currently being serviced by buses, including: Bus Stop Code, location coordinates.

resource_url <- "http://datamall2.mytransport.sg/ltaodataservice/BusStops"

all_data <- list()
skip_value <- 0

while (TRUE) {
  res <- GET(paste0(resource_url, "?$skip=", skip_value), 
             add_headers(AccountKey = Sys.getenv("LTA_KEY"), accept = "application/json"))
  
  if (res$status_code == 200) {
    res_text <- content(res, as = "text")
    res_list <- fromJSON(res_text, flatten = TRUE)
    
    if (length(res_list$value) == 0) {
      break  # Exit loop when there are no more observations
    }
    
    all_data[[length(all_data) + 1]] <- res_list$value
    skip_value <- skip_value + 500
  } else {
    stop("Failed to retrieve data. Status code: ", res$status_code)
  }
}

bus_stops <- bind_rows(all_data)


# Passenger Volume for Buses (February)

resource_url <- "http://datamall2.mytransport.sg/ltaodataservice/PV/Bus?Date=202402"
res = GET(resource_url, add_headers(AccountKey = Sys.getenv("LTA_KEY"),accept = "application/json"))
res$status_code
res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
res_list[["value"]][["Link"]]


# Passenger Volume for Buses (March)

resource_url <- "http://datamall2.mytransport.sg/ltaodataservice/PV/Bus?Date=202403"
res = GET(resource_url, add_headers(AccountKey = Sys.getenv("LTA_KEY"),accept = "application/json"))
res$status_code
res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
res_list[["value"]][["Link"]]


# Passenger Volume by Origin Destination Bus Stops (February)

resource_url <- "http://datamall2.mytransport.sg/ltaodataservice/PV/ODBus?Date=202402"
res = GET(resource_url, add_headers(AccountKey = Sys.getenv("LTA_KEY"),accept = "application/json"))
res$status_code
res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
res_list[["value"]][["Link"]]


# Passenger Volume by Origin Destination Bus Stops (March)

resource_url <- "http://datamall2.mytransport.sg/ltaodataservice/PV/ODBus?Date=202403"
res = GET(resource_url, add_headers(AccountKey = Sys.getenv("LTA_KEY"),accept = "application/json"))
res$status_code
res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
res_list[["value"]][["Link"]]


# Passenger Volume by Origin Destination Train Stations (February)

resource_url <- "http://datamall2.mytransport.sg/ltaodataservice/PV/ODTrain?Date=202402"
res = GET(resource_url, add_headers(AccountKey = Sys.getenv("LTA_KEY"),accept = "application/json"))
res$status_code
res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
res_list[["value"]][["Link"]]

# Passenger Volume by Origin Destination Train Stations (March)

resource_url <- "http://datamall2.mytransport.sg/ltaodataservice/PV/ODTrain?Date=202403"
res = GET(resource_url, add_headers(AccountKey = Sys.getenv("LTA_KEY"),accept = "application/json"))
res$status_code
res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
res_list[["value"]][["Link"]]


# Passenger Volume for Trains (March)

resource_url <- "http://datamall2.mytransport.sg/ltaodataservice/PV/Train?Date=202403"
res = GET(resource_url, add_headers(AccountKey = Sys.getenv("LTA_KEY"),accept = "application/json"))
res$status_code
res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
res_list[["value"]][["Link"]]

# Passenger Volume for Trains (February)

resource_url <- "http://datamall2.mytransport.sg/ltaodataservice/PV/Train?Date=202402"
res = GET(resource_url, add_headers(AccountKey = Sys.getenv("LTA_KEY"),accept = "application/json"))
res$status_code
res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
res_list[["value"]][["Link"]]


# Taxi Availability
## Returns location coordinates of all Taxis that are currently available for hire. Does not include "Hired" or "Busy" Taxis.

resource_url <- "http://datamall2.mytransport.sg/ltaodataservice/Taxi-Availability"

all_data <- list()
skip_value <- 0

while (TRUE) {
  res <- GET(paste0(resource_url, "?$skip=", skip_value), 
             add_headers(AccountKey = Sys.getenv("LTA_KEY"), accept = "application/json"))
  
  if (res$status_code == 200) {
    res_text <- content(res, as = "text")
    res_list <- fromJSON(res_text, flatten = TRUE)
    
    if (length(res_list$value) == 0) {
      break  # Exit loop when there are no more observations
    }
    
    all_data[[length(all_data) + 1]] <- res_list$value
    skip_value <- skip_value + 500
  } else {
    stop("Failed to retrieve data. Status code: ", res$status_code)
  }
}

taxi_avail <- bind_rows(all_data)


# Taxi Stands
## Returns detailed information of Taxi stands, such as location and whether is it barrier free.

resource_url <- "http://datamall2.mytransport.sg/ltaodataservice/TaxiStands"
res = GET(resource_url, add_headers(AccountKey = Sys.getenv("LTA_KEY"),accept = "application/json"))
res$status_code
res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
taxi_stands = data.frame(res_list[2])


# Estimated Travel Times
## Returns estimated travel times of expressways (in segments).

resource_url <- "http://datamall2.mytransport.sg/ltaodataservice/EstTravelTimes"

all_data <- list()
skip_value <- 0

while (TRUE) {
  res <- GET(paste0(resource_url, "?$skip=", skip_value), 
             add_headers(AccountKey = Sys.getenv("LTA_KEY"), accept = "application/json"))
  
  if (res$status_code == 200) {
    res_text <- content(res, as = "text")
    res_list <- fromJSON(res_text, flatten = TRUE)
    
    if (length(res_list$value) == 0) {
      break  # Exit loop when there are no more observations
    }
    
    all_data[[length(all_data) + 1]] <- res_list$value
    skip_value <- skip_value + 500
  } else {
    stop("Failed to retrieve data. Status code: ", res$status_code)
  }
}

est_travel_time <- bind_rows(all_data)


# Daily Platform Crowd Density Forecast 30min Intervals
## CCL 
### Circle Line

resource_url = "http://datamall2.mytransport.sg/ltaodataservice/PCDForecast?TrainLine=CCL"
res = GET(resource_url, add_headers(AccountKey = Sys.getenv("LTA_KEY"),accept = "application/json"))
res$status_code
res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
stations = res_list$value$Stations[[1]]
ccl_forecast = data.frame()
for (i in 1:length(stations[[1]])){
  stn = rep(stations[[1]][i],length(stations[[2]][[i]]))
  ccl_forecast = rbind(ccl_forecast,cbind(stn,stations[[2]][[i]]))
}


## CEL
### Circle Line Extension - Bayfront, Marina Bay

resource_url = "http://datamall2.mytransport.sg/ltaodataservice/PCDForecast?TrainLine=CEL"
res = GET(resource_url, add_headers(AccountKey = Sys.getenv("LTA_KEY"),accept = "application/json"))
res$status_code
res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
stations = res_list$value$Stations[[1]]
cel_forecast = data.frame()
for (i in 1:length(stations[[1]])){
  stn = rep(stations[[1]][i],length(stations[[2]][[i]]))
  cel_forecast = rbind(cel_forecast,cbind(stn,stations[[2]][[i]]))
}


## CGL
### Changi Extension - Expo, Changi Airport

resource_url = "http://datamall2.mytransport.sg/ltaodataservice/PCDForecast?TrainLine=CGL"
res = GET(resource_url, add_headers(AccountKey = Sys.getenv("LTA_KEY"),accept = "application/json"))
res$status_code
res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
stations = res_list$value$Stations[[1]]
cgl_forecast = data.frame()
for (i in 1:length(stations[[1]])){
  stn = rep(stations[[1]][i],length(stations[[2]][[i]]))
  cgl_forecast = rbind(cgl_forecast,cbind(stn,stations[[2]][[i]]))
}


## DTL
### Downtown Line

resource_url = "http://datamall2.mytransport.sg/ltaodataservice/PCDForecast?TrainLine=DTL"
res = GET(resource_url, add_headers(AccountKey = Sys.getenv("LTA_KEY"),accept = "application/json"))
res$status_code
res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
stations = res_list$value$Stations[[1]]
dtl_forecast = data.frame()
for (i in 1:length(stations[[1]])){
  stn = rep(stations[[1]][i],length(stations[[2]][[i]]))
  dtl_forecast = rbind(dtl_forecast,cbind(stn,stations[[2]][[i]]))
}


## EWL
### East West Line

resource_url = "http://datamall2.mytransport.sg/ltaodataservice/PCDForecast?TrainLine=EWL"
res = GET(resource_url, add_headers(AccountKey = Sys.getenv("LTA_KEY"),accept = "application/json"))
res$status_code
res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
stations = res_list$value$Stations[[1]]
ewl_forecast = data.frame()
for (i in 1:length(stations[[1]])){
  stn = rep(stations[[1]][i],length(stations[[2]][[i]]))
  ewl_forecast = rbind(ewl_forecast,cbind(stn,stations[[2]][[i]]))
}


## NEL
### North East Line

resource_url = "http://datamall2.mytransport.sg/ltaodataservice/PCDForecast?TrainLine=NEL"
res = GET(resource_url, add_headers(AccountKey = Sys.getenv("LTA_KEY"),accept = "application/json"))
res$status_code
res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
stations = res_list$value$Stations[[1]]
nel_forecast = data.frame()
for (i in 1:length(stations[[1]])){
  stn = rep(stations[[1]][i],length(stations[[2]][[i]]))
  nel_forecast = rbind(nel_forecast,cbind(stn,stations[[2]][[i]]))
}


## NSL
### North South Line

resource_url = "http://datamall2.mytransport.sg/ltaodataservice/PCDForecast?TrainLine=NSL"
res = GET(resource_url, add_headers(AccountKey = Sys.getenv("LTA_KEY"),accept = "application/json"))
res$status_code
res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
stations = res_list$value$Stations[[1]]
nsl_forecast = data.frame()
for (i in 1:length(stations[[1]])){
  stn = rep(stations[[1]][i],length(stations[[2]][[i]]))
  nsl_forecast = rbind(nsl_forecast,cbind(stn,stations[[2]][[i]]))
}


## BPL
### Bukit Panjang LRT

resource_url = "http://datamall2.mytransport.sg/ltaodataservice/PCDForecast?TrainLine=BPL"
res = GET(resource_url, add_headers(AccountKey = Sys.getenv("LTA_KEY"),accept = "application/json"))
res$status_code
res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
stations = res_list$value$Stations[[1]]
bpl_forecast = data.frame()
for (i in 1:length(stations[[1]])){
  stn = rep(stations[[1]][i],length(stations[[2]][[i]]))
  bpl_forecast = rbind(bpl_forecast,cbind(stn,stations[[2]][[i]]))
}


## SLRT
### Sengkang LRT

resource_url = "http://datamall2.mytransport.sg/ltaodataservice/PCDForecast?TrainLine=SLRT"
res = GET(resource_url, add_headers(AccountKey = Sys.getenv("LTA_KEY"),accept = "application/json"))
res$status_code
res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
stations = res_list$value$Stations[[1]]
slrt_forecast = data.frame()
for (i in 1:length(stations[[1]])){
  stn = rep(stations[[1]][i],length(stations[[2]][[i]]))
  slrt_forecast = rbind(slrt_forecast,cbind(stn,stations[[2]][[i]]))
}


## PLRT
### Punggol LRT

resource_url = "http://datamall2.mytransport.sg/ltaodataservice/PCDForecast?TrainLine=PLRT"
res = GET(resource_url, add_headers(AccountKey = Sys.getenv("LTA_KEY"),accept = "application/json"))
res$status_code
res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
stations = res_list$value$Stations[[1]]
plrt_forecast = data.frame()
for (i in 1:length(stations[[1]])){
  stn = rep(stations[[1]][i],length(stations[[2]][[i]]))
  plrt_forecast = rbind(plrt_forecast,cbind(stn,stations[[2]][[i]]))
}


# Geography of Island

library(sf)
shape = st_make_valid(st_read("../data/MasterPlan2019SubzoneBoundaryNoSeaGEOJSON.geojson"))
ggplot(data = shape) +
  geom_sf(aes(geometry = geometry), fill = "lightgray", color = "white") +
  labs(title = "Subzone bounaries in Master Plan 2019")


# Train Stations in Singapore (having trouble plotting)

shape_train = st_make_valid(st_read("../data/TrainStation_Feb2023/RapidTransitSystemStation.shp"))

ggplot() +
  geom_sf(data = shape, aes(geometry = geometry), fill = "steelblue", alpha = 0.3,
          color = "white") + labs(title = "Subzone bounaries in Master
                                  Plan 2019") +
  geom_sf(data = shape_train, aes(geometry = geometry), color = "darkblue")



# Bus Stops in Singapore

shape_bus <- st_read("../data/BusStopLocation_Jul2023/BusStop.shp")
ggplot() +
  geom_sf(data = shape, aes(geometry = geometry), fill = "steelblue", alpha = 0.3,
          color = "white") + labs(title = "Subzone bounaries in Master
                                  Plan 2019") +
  geom_sf(data = shape_bus, aes(geometry = geometry), color = "darkblue")

