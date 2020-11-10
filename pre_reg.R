rm(list = ls(all = TRUE)) # clear all variables in the environment
setwd("E:/601_proj") ##please replace this as your own workspace
library(tidyverse)
library(dplyr)
library(data.table)
library(zoo)
rawdata5<-readRDS("climate_case_daily.rds")
reg_data<-readRDS("for_regression_2.rds")
rawdata3<-read.csv("us_cities_population_wiki.csv")
names(rawdata3)[1] <- 'rank_2019'
head(rawdata5)

#filter out missing data
rank_kept<-which(table(rawdata5$rank_2019)>300)
data_kept<-rawdata5[rawdata5$rank_2019 %in% rank_kept,]

#daily cases
cases<-data_kept$cases
cases[is.na(cases)]<-0
cases2<-c(0,cases[1:(length(cases)-1)])
daily_cases<-cases-cases2
daily_cases[daily_cases<0]<-0
data_kept$daily_cases<-daily_cases

#avetemp
avg_week_weather <- data_kept %>% 
  #filter(!is.na(avg_day_tempf)&!is.na(avg_day_humid)&!is.na(avg_day_vsby)) %>% 
  group_by(city) %>% 
  mutate(
    avg_week_tempf = rollmean(meantmpf,k=14,fill = NA,na.pad = T,align = "right"),
    avg_week_humid = rollmean(relh,14,fill = NA,na.pad = T,align = "right"),
    avg_week_vsby = rollmean(vsby,14,fill = NA,na.pad = T,align = "right")
  ) %>% 
  ungroup()

##add missing data whose size is between 2 and 300
missing_ones<-rep()
i=1
for (k in c(1:100)){
  data<-filter(rawdata5,rank_2019==k)
  total<-length(data$date)
  if (total<=300 & total>1){
    missing_ones[i]<-data$rank_2019
    i=i+1
  }
  k=k+1
} 

for (m in missing_ones){
  data<-filter(rawdata5,rank_2019==m)
  break_point<-rep(1,)
  for (i in c(2:length(data$date))){
    if (data[i,'date']-data[i-1,'date']!=1){
      break_point<-append(break_point,i)
    }
    i=i+1
  }
  week_tempf<-rep()
  week_humid<-rep()
  week_vsby<-rep()
  daily_cases_n<-rep()
  for (i in c(1:length(break_point))){
    if (i!=length(break_point)){
      data_n<-data[break_point[i]:break_point[i+1]-1,]
      #daily cases
      cases<-data_n$cases
      cases[is.na(cases)]<-0
      cases2<-c(0,cases[1:(length(cases)-1)])
      daily_cases<-cases-cases2
      daily_cases[daily_cases<0]<-0
      data_n$daily_cases<-daily_cases
      avg_week_tempf = rollmean(data_n$meantmpf,k=14,fill = NA,na.pad = T,align = "right")
      avg_week_humid = rollmean(data_n$relh,14,fill = NA,na.pad = T,align = "right")
      avg_week_vsby = rollmean(data_n$vsby,14,fill = NA,na.pad = T,align = "right")
      # ) 
      week_tempf<-append(week_tempf,avg_week_tempf)
      week_humid<-append(week_humid,avg_week_humid)
      week_vsby<-append(week_vsby,avg_week_vsby)
      daily_cases_n<-append(daily_cases_n,daily_cases)
    }
    else {
      data_n<-data[break_point[i]:length(data$date),]
      #daily cases
      cases<-data_n$cases
      cases[is.na(cases)]<-0
      cases2<-c(0,cases[1:(length(cases)-1)])
      daily_cases<-cases-cases2
      daily_cases[daily_cases<0]<-0
      data_n$daily_cases<-daily_cases
      avg_week_tempf = rollmean(data_n$meantmpf,k=14,fill = NA,na.pad = T,align = "right")
      avg_week_humid = rollmean(data_n$relh,14,fill = NA,na.pad = T,align = "right")
      avg_week_vsby = rollmean(data_n$vsby,14,fill = NA,na.pad = T,align = "right")
      # ) 
      week_tempf<-append(week_tempf,avg_week_tempf)
      week_humid<-append(week_humid,avg_week_humid)
      week_vsby<-append(week_vsby,avg_week_vsby)
      daily_cases_n<-append(daily_cases_n,daily_cases)
    }
    i=i+1
  }
  data$avg_week_tempf<-week_tempf[1:length(data$date)]
  data$avg_week_humid<-week_humid[1:length(data$date)]
  data$avg_week_vsby<-week_vsby[1:length(data$date)]
  data$daily_cases<-daily_cases_n[1:length(data$date)]
  avg_week_weather<-rbind(avg_week_weather,data)
}

#first 100
first_hundred <- avg_week_weather %>% 
  filter(daily_cases>=100) %>% 
  arrange(city,date) %>% 
  distinct(city,.keep_all = T)

#combine population area and state, filter 100
pop<-unique(reg_data[,c("rank_2019","land_area_2016_kmmi","pop_density_2016_sq_km")])
state<-unique(rawdata3[,c("rank_2019","state")])
pop$rank_2019<-as.numeric(pop$rank_2019)
reg_data2 <- avg_week_weather %>% 
  left_join(state)%>%
  left_join(pop) %>% 
  
  left_join(first_hundred %>% select(city,first_hundred_date = date),
            by = "city") %>% 
  mutate(
    log_cases = log(daily_cases/land_area_2016_kmmi)
  )%>% 
  mutate(
    cases_per_km = daily_cases/land_area_2016_kmmi
  )%>% 
  filter(date>=first_hundred_date & log_cases>=-100000)

par(mfrow=c(1,2))
hist(reg_data2$cases_per_km)
hist(reg_data2$log_cases)
attach(reg_data)
mod<-lm(log_cases~avg_week_tempf+avg_week_humid+avg_week_vsby+pop_density_2016_sq_km+state,data=reg_data2)
summary(mod)
par(mfrow=c(2,2))
plot(mod)
