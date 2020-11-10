###############################################################################################
# STAT 601 Final Project
# Sam Tauke 11/1/20
# The Purpose of This File is to Import and Combine Data for Regression Analysis
###############################################################################################

rm(list = ls(all = TRUE)) # clear all variables in the environment
setwd("C:/Users/samta/Documents/R/STAT 601/final_project")

library(ggpubr)
library(tidyverse)
library(readxl)
library(lubridate)
library(rvest)
library(dplyr)
library(jsonlite)
library(httr)
library(RCurl)
library(stringr)
library(riem)  #Data from IOWA Mesonet: https://cran.r-project.org/web/packages/riem/riem.pdf 
library(geosphere)
library(sp)
library(maps)
library(maptools)
library(data.table)
library(janitor)


#cities_with_weather <- read_rds("cities_with_weather.rds")



# Build a Dataset of All US Stations --------------------------------------

us_states <- state.name

all_networks <- riem_networks()

us_networks <- all_networks %>% 
  filter(!grepl("__",code) &
           !grepl("CA ASOS",name)&
           !grepl("CANADA ASOS",name,ignore.case = T)
         ) %>% 
  pull(code)



us_stations <- data.frame(
                id=character(),
                 name=character(), 
                 lon=numeric(),
                lat = numeric(),
                 stringsAsFactors=FALSE)

station_puller <- function(network){
  temp_df <- riem_stations(network)
  
  us_stations <- us_stations %>% 
    bind_rows(temp_df) %>% 
    return()
}



us_stations <- lapply(us_networks, station_puller) %>% 
  bind_rows()




# Import Hundred Largest Cities in the US -----------------------------------
us_cities_wiki <- read_xlsx("us_cities_population_wiki.xlsx")




# Cross Airport List with Cities List, find minimum distances -------------


clean_cities <- us_cities_wiki %>% 
  filter(rank_2019<=100) %>% 
  mutate(
    lat = as.numeric(gsub("°N","",str_extract(location,"\\d+.\\d+°N"))),
    lon = -as.numeric(gsub("°W","",str_extract(location,"\\d+.\\d+°W")))
  )


temp_cities <- clean_cities %>% 
  select(city,lon_city=lon,lat_city=lat)

temp_stations <- us_stations %>% 
  select(station_id=id,lon_station = lon,lat_station=lat)

cities_nest <- nest(temp_cities,-city, .key = 'city_coords')
station_nest <- nest(temp_stations,-station_id ,.key = 'station_coords')


master_list <- crossing(cities_nest,station_nest)

distance <- master_list %>% 
  mutate(
    dist = map2_dbl(city_coords, station_coords, distm)) %>% 
    filter(!(city=="Chicago"&station_id=="CGX") & 
             !(city=="Durham"&station_id=="IGX") &
             !(city=="Honolulu[b]"&station_id=="PHIK") &
             !(city=="Irvine"&station_id=="NZJ")) %>% #Drop Weather station since it doesnt have weather data
    group_by(city) %>% 
    filter(dist == min(dist))


cities_with_station <- clean_cities %>% 
  left_join(distance %>% select(city,closest_station=station_id,dist),
            by="city") %>% 
  left_join(us_stations %>% select(closest_station = id,station_lat=lat,station_lon=lon)) %>% 
  mutate(
    closest_station
  )




# Pull Weather Data for Each Day from 1/1/20-10/31/20 ---------------------


closest_stations <- cities_with_station %>% 
  filter(rank_2019<=100) %>% 
  distinct(closest_station) %>% 
  pull(closest_station)


weather_getter <- function(station_input){
  riem_measures(station = station_input,date_start = "2020-01-01",date_end = "2020-10-31")
}


mega_weather <- lapply(cities_with_station$closest_station,weather_getter)

iter_end = length(mega_weather)


combined_tibble <- mega_weather[1] %>% 
  bind_rows() %>% 
  mutate_all(as.character)
for(i in 2:100) {
  test <- mega_weather[i] %>% bind_rows() %>% 
    mutate_all(as.character)
  combined_tibble <- combined_tibble %>% 
    bind_rows(test)
} 

distinct_weather <- combined_tibble %>% 
  distinct(station,valid,.keep_all = T)


cities_with_weather <- cities_with_station %>% 
  left_join(distinct_weather,
            by = c("closest_station"="station")) %>% 
  rename(
    lat = lat.x,
    lon = lon.x
  ) %>% 
  select(-lat.y,-lon.y)




# Pull COVID19 Data -------------------------------------------------------

nyt_covid_county <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# Find first day on which a county had 100 cases:
first_hundred <- nyt_covid_county %>% 
  filter(cases>=100) %>% 
  arrange(date)


#Match County Names to City

latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}

# Test the function using points in Wisconsin and Oregon.
city_points <- data.frame(x = clean_cities$lon, y = clean_cities$lat)

city_county_list <- latlong2county(city_points)

clean_city_county <- clean_cities %>% 
  bind_cols(city_county_list) %>% 
  mutate(
    county = gsub(",","",str_extract(...14,",.+")),
    county = case_when(
      grepl("San Francisco",city,ignore.case = T) ~ "san francisco",
      grepl("Boston",city,ignore.case = T) ~ "suffolk",
      grepl("Seattle",city,ignore.case = T) ~ "king",
      grepl("Honolulu",city,ignore.case = T) ~ "honolulu",
      grepl("Corpus Christi",city,ignore.case = T) ~ "nueces",
      grepl("Anchorage",city,ignore.case = T) ~ "anchorage",
      grepl("Jersey City",city,ignore.case = T) ~ "hudson",
      grepl("New York City",city,ignore.case = T) ~ "new york city", #Treated independently by NYT Data
      grepl("Washington",city,ignore.case = T) ~ "district of columbia", #Treated independently by NYT Data
      grepl("Virginia Beach",city,ignore.case = T) ~ "virginia beach city", #Treated independently by NYT Data
      grepl("St. Louis",city,ignore.case = T) ~ "st. louis", #Treated independently by NYT Data
      grepl("Chesapeake",city,ignore.case = T) ~ "chesapeake city", #Treated independently by NYT Data
      grepl("Norfolk",city,ignore.case = T) ~ "norfolk city", #Treated independently by NYT Data
      TRUE ~ county
    )
  )


# Merge City List with NYT Data

nyt_merge <- nyt_covid_county %>%  
  mutate(
         county = trimws(tolower(county),which = "both"),
         state = trimws(tolower(state),which = "both")
         )
         
         
         
clean_with_covid <- clean_city_county %>% 
  mutate(
    state = trimws(tolower(state),which = "both"),
    county = trimws(tolower(county),which = "both")
    )%>% 
  left_join(nyt_merge,
            by = c("county","state")
    ) %>% 
  select(city,state,county,date,fips,cases,deaths)

# Query AQI API - Archived due to Rate Limiting
# Data Pulled from: https://docs.airnowapi.org/forecastsbylatlon/query


# cities_for_aqi <- clean_cities %>% 
#   mutate(
#     aqi_url = paste0("https://www.airnowapi.org/aq/observation/latLong/historical/?format=text/csv&latitude=",as.character(lat),"&longitude=",as.character(lon),"&date=2020-11-09T00-0000&distance=25&API_KEY=5DACE46C-2E9D-4D0E-99F8-C345C952E73A")
#   ) %>% 
#   select(
#     city, aqi_url
#   )
# 
# 
# 
# aqi_machine <- function(url){
#   temp <- GET(url)
#   
#   content(temp,"parsed") %>% 
#     bind_rows() %>% 
#     mutate_all(.,~as.character(.)) %>% 
#     mutate(
#       aqi_url = url
#     ) %>%
#     return()
# }
# 
# 
# combined_aqis <- lapply(cities_for_aqi$aqi_url[1],aqi_machine) %>% 
#   bind_rows()
# 
# 
# 
# 
# clean_with_aqi <- clean_cities %>% 
#   select(city,lat,lon) %>% 
#   mutate(
#     lat = round(lat,2),
#     lon = round(lon,3)
#     ) %>% 
#   left_join(combined_aqis %>% filter(ParameterName=="PM2.5") %>% 
#               mutate(lat = round(as.numeric(Latitude),3),
#                      lon = round(as.numeric(Longitude),3)
#                      ) %>% 
#               select(aqi_date=DateObserved,aqi=AQI,aqi_parameter=ParameterName,lat,lon),
#             by=c("lat","lon")
#             )



# Match Each city with closest aqi station---------------------

locations_list <- read.table("https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow/2020/20200101/HourlyAQObs_2020010123.dat",
                           sep = ",",
                           header = TRUE) %>% 
  clean_names()


temp_stations_aqi <- locations_list %>%
  filter(country_code=="US") %>% 
  select(aqsid,lon_station = longitude,lat_station=latitude)

cities_nest <- nest(temp_cities,-city, .key = 'city_coords')
station_nest_aqi <- nest(temp_stations_aqi,-aqsid ,.key = 'station_coords')


master_list_aqi <- crossing(cities_nest,station_nest_aqi)

distance_aqi_master <- master_list_aqi %>% 
  mutate(
    dist = map2_dbl(city_coords, station_coords, distm))


distance_aqi_top3 <- distance_aqi_master %>% 
  arrange(dist) %>% 
  group_by(city) %>% 
  mutate(
    rank = row_number()
  ) %>% 
  ungroup() %>% 
  filter(rank<=3)
  

cities_with_station_aqi <- clean_cities %>% 
  left_join(distance_aqi_top3 %>% select(city,aqsid,dist),
            by="city") %>% 
  left_join(locations_list %>% select(aqsid,station_lat=latitude,station_lon=longitude))




# Pull and Stack all AQI -----------------------------------------------------------


days_2020 <- seq(from = as.Date("2020-03-01"),to=as.Date("2020-10-31"),by = 1) %>% 
  as.tibble() %>% 
  mutate(
    url = paste0("https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow/2020/",gsub("-","",as.character(value)),"/HourlyAQObs_",gsub("-","",as.character(value)),"23.dat")
  )






aqi_day_machine <- function(url){
  
  read.table(url,
             sep = ",",
             header = TRUE) %>% 
    clean_names() %>% 
    return()
}


temp_vector <- days_2020$url
temp_short <- temp_vector[1:3]


aqi_day_data_23 <- lapply(days_2020$url,aqi_day_machine) %>% 
  bind_rows()


# Pull Data for closest three stations ------------------------------------

cities_with_aqi <- cities_with_station_aqi %>% 
  left_join(aqi_day_data_23 %>% select(aqsid,valid_date,valid_time,ozone_aqi:pm10_unit))

# Write to RDS ------------------------------------------------------------
    
write_rds(cities_with_station,"cities_with_station.rds")
write_rds(cities_with_weather,"cities_with_weather.rds")
write_rds(clean_with_covid,"cities_w_nyt_covid.rds")

write_rds(cities_with_aqi,"cities_with_aqi.rds")


temp <- read_rds("cities_w_nyt_covid.rds")
temp2 <- temp %>% 
  filter(cases>=100)


