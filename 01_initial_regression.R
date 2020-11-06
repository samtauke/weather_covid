###############################################################################################
# STAT 601 Final Project
# Sam Tauke 11/5/20
# The Purpose of This File is to Run the Basic Regression
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
library(RCurl)
library(stringr)
library(riem)  #Data from IOWA Mesonet: https://cran.r-project.org/web/packages/riem/riem.pdf 
library(geosphere)
library(sp)
library(maps)
library(maptools)
library(zoo)

cities_with_weather <- read_rds("cities_with_weather.rds")
cities_w_covid <- read_rds("cities_w_nyt_covid.rds")



# Average Daily Weather

avg_daily_weather <- cities_with_weather %>% 
  mutate(
    date = as.Date(valid)
  ) %>% 
  group_by(city,date) %>% 
  summarise(
    avg_day_tempf = mean(as.numeric(tmpf),na.rm = T),
    avg_day_humid = mean(as.numeric(relh),na.rm = T),
    avg_day_vsby = mean(as.numeric(vsby),na.rm = T)
  ) %>% 
  ungroup() 



avg_week_weather <- avg_daily_weather %>% 
  #filter(!is.na(avg_day_tempf)&!is.na(avg_day_humid)&!is.na(avg_day_vsby)) %>% 
  group_by(city) %>% 
  mutate(
    avg_week_tempf = rollmean(avg_day_tempf,k=14,fill = NA,na.pad = T,align = "right"),
    avg_week_humid = rollmean(avg_day_humid,14,fill = NA,na.pad = T,align = "right"),
    avg_week_vsby = rollmean(avg_day_vsby,14,fill = NA,na.pad = T,align = "right")
  ) %>% 
  ungroup()



first_hundred <- cities_w_covid %>% 
  group_by(city) %>% 
  mutate(
    day_cases = cases - lag(cases)
  ) %>% 
  ungroup() %>% 
  filter(day_cases>=100) %>% 
  arrange(city,date) %>% 
  distinct(city,.keep_all = T)

reg_data <- cities_w_covid %>% 
  left_join(avg_week_weather %>% select(-contains("day")),
            by = c("city", "date")) %>% 
  left_join(cities_with_weather %>% distinct(city,pop_density_2016_sq_km,land_area_2016_kmmi)) %>% 
  left_join(first_hundred %>% select(city,first_hundred_date = date),
            by = "city") %>% 
  group_by(city) %>% 
  mutate(
    day_cases = cases - lag(cases)
    ) %>% 
  ungroup() %>% 
  mutate(
    sq_km = as.numeric(gsub("[^0-9.]", "",land_area_2016_kmmi)),
    log_cases = log(day_cases/sq_km),
    pop_dens_2016 = as.numeric(gsub(",","",gsub("/km2","",pop_density_2016_sq_km)))
  ) %>% 
  filter(date>=first_hundred_date & log_cases>=-100000)



reg_model <- lm(log_cases ~ pop_dens_2016 + avg_week_tempf + avg_week_humid + avg_week_vsby,data = reg_data)


summary(reg_model)




