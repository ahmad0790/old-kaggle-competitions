detach(package: plyr)
library(mefa)
library(lubridate)
library(dplyr)
library(zoo)

path <- "/Users/ahkhan/Documents/Kaggle/West Nile/"
train = read.csv(paste0(path,"train.csv"),header=TRUE,stringsAsFactors = T)
test = read.csv(paste0(path,"test.csv"),header=TRUE,stringsAsFactors = T)
weather = read.csv(paste0(path,"weather.csv"),header=TRUE,stringsAsFactors = T)
spray = read.csv(paste0(path,"spray.csv"),header=TRUE)
subm = read.csv(paste0(path,"sampleSubmission.csv"),header=TRUE,stringsAsFactors = F)

weather[(weather == " ")] <- NA
weather[(weather == "M")] <- NA
weather[(weather == "-")] <- NA
weather[(weather == "T")] <- NA
weather[(weather == " T")] <- NA
weather[(weather == "  T")] <- NA

weather$Water1 = NULL
weather$Depth = NULL
weather$SnowFall = NULL
weather$Sunrise = NULL
weather$Sunset = NULL
weather$Depart = NULL

#Get the nearest station
train$Station <- ifelse((((train$Latitude-41.995)^2 + (train$Longitude + 87.933)^2) < 
                           ((train$Latitude-41.786)^2 + (train$Longitude + 87.752)^2)),1,2)

test$Station <- ifelse((((test$Latitude-41.995)^2 + (test$Longitude + 87.933)^2) < 
                          ((test$Latitude-41.786)^2 + (test$Longitude + 87.752)^2)),1,2)

w1 = weather[weather$Station ==1,]
w2 = weather[weather$Station ==2,]

#Replace NA's with the nearest value above
W1 <- rbind(w1[2,],w1)
W1 <- fill.na(W1) 
W1 <- W1[-1,]
rownames(W1) <- NULL

W2 <- rbind(w2[2,],w2)
W2 <- fill.na(W2) 
W2 <- W2[-1,]
rownames(W2) <- NULL

Weather <- rbind(W1,W2)
Weather$year <- as.factor(as.character(substr(Weather$Date,1,4)))
Weather$week <- as.integer(week(as.Date(Weather$Date)))

for(i in c(3:9,11:ncol(Weather))){
  Weather[,i] <- as.numeric(Weather[,i])
}
Weather[,10] <- factor(Weather[,10])

Weather$day<-as.numeric(day(as.Date(Weather$Date)))
Weather$dayofyear<-as.numeric(yday(as.Date(Weather$Date))) 

##END###
#Weather <- 
#Weather %>%
#group_by(Station, year)%>%
#arrange(Station,Date)%>%
#mutate(
  #Tmax_weekly = rollmean(x=Tmax,14,align="right",fill = NA),
  #Tmin_weekly = rollmean(x=Tmin,14,align="right",fill = NA),
  #Tavg_weekly = rollmean(x=Tavg,14,align="right",fill = NA),
  #Precip_week = rollsum(x=PrecipTotal,14,align="right",fill = NA)
  #)

#Weather <- 
#Weather %>%
#group_by(Station, year)%>%
#arrange(Station,Date)%>%
#mutate(Tmax_lag_week = lag(Tmax_weekly,7),
#,Tmax_lag_2week = lag(Tmax_weekly,14),
#Tmax_lag_3week = lag(Tmax_weekly,21)

# Tmin_lag_week = lag(Tmin_weekly,7),
#,Tmin_lag_2week = lag(Tmin_weekly,14),
#Tmin_lag_3week = lag(Tmin_weekly,21))

#Precip_lag_week = lag(Precip_week,7),
#,Precip_lag_2week = lag(Precip_week,14),
#Precip_lag_3week = lag(Precip_week,21))

#Tavg_lag_week = lag(Tavg_week,7),
#AvgSpeed_lag_week = lag(AvgSpeed_week,7))

##CONTINUE
Weather <- 
  Weather %>%
  group_by(Station, year)%>%
  arrange(Station,Date)%>%
  mutate(Tmax_lag1 = lag(Tmax,1),
         Tmax_lag2 = lag(Tmax,2),
         Tmax_lag3 = lag(Tmax,3),
         Tmax_lag5 = lag(Tmax,5),
         Tmax_lag8 = lag(Tmax,8),
         Tmax_lag12 = lag(Tmax,12),
         
         Tmin_lag1 = lag(Tmin,1),
         Tmin_lag2 = lag(Tmin,2),
         Tmin_lag3 = lag(Tmin,3),
         Tmin_lag5 = lag(Tmin,5),
         Tmin_lag8 = lag(Tmin,8),
         Tmin_lag12 = lag(Tmin,12),
         
         Tavg_lag1 = lag(Tavg,1),
         Tavg_lag2 = lag(Tavg,2),
         Tavg_lag3 = lag(Tavg,3),
         Tavg_lag5 = lag(Tavg,5),
         Tavg_lag8 = lag(Tavg,8),
         Tavg_lag12 = lag(Tavg,12),
         
         PrecipTotal_lag1 = lag(PrecipTotal,1),
         PrecipTotal_lag2 = lag(PrecipTotal,2),
         PrecipTotal_lag3 = lag(PrecipTotal,3),
         PrecipTotal_lag5 = lag(PrecipTotal,5),
         PrecipTotal_lag8 = lag(PrecipTotal,8),
         PrecipTotal_lag12 = lag(PrecipTotal,12),
         
         AvgSpeed_lag1 = lag(AvgSpeed,1),
         AvgSpeed_lag2 = lag(AvgSpeed,2),
         AvgSpeed_lag3 = lag(AvgSpeed,3),
         AvgSpeed_lag5 = lag(AvgSpeed,5),
         AvgSpeed_lag8 = lag(AvgSpeed,8),
         AvgSpeed_lag12 = lag(AvgSpeed,12),
         
         DewPoint_lag1 = lag(DewPoint,1),
         DewPoint_lag2 = lag(DewPoint,2),
         DewPoint_lag3 = lag(DewPoint,3),
         DewPoint_lag5 = lag(DewPoint,5),
         DewPoint_lag8 = lag(DewPoint,8),
         DewPoint_lag12 = lag(DewPoint,12),
         
         WetBulb_lag1 = lag(WetBulb,1),
         WetBulb_lag2 = lag(WetBulb,2),
         WetBulb_lag3 = lag(WetBulb,3),
         WetBulb_lag5 = lag(WetBulb,5),
         Wetbulb_lag8 = lag(WetBulb,8),
         WetBulb_lag12 = lag(WetBulb,12))

Weather <- 
  Weather %>%
  group_by(Station,week)%>%
  mutate(Tmax_lag_1_sd = sd(Tmax_lag1,na.rm=TRUE),
         Tmax_lag_2_sd = sd(Tmax_lag2,na.rm=TRUE),
         Tmax_lag_3_sd = sd(Tmax_lag3,na.rm=TRUE),
         Tmax_lag_5_sd = sd(Tmax_lag5,na.rm=TRUE),
         Tmax_lag_8_sd = sd(Tmax_lag8,na.rm=TRUE),
         Tmax_lag_12_sd = sd(Tmax_lag12,na.rm=TRUE),
         Tmin_lag_1_sd = sd(Tmin_lag1,na.rm=TRUE),
         Tmin_lag_2_sd = sd(Tmin_lag2,na.rm=TRUE),
         Tmin_lag_3_sd = sd(Tmin_lag3,na.rm=TRUE),
         Tmin_lag_5_sd = sd(Tmin_lag5,na.rm=TRUE),
         Tmin_lag_8_sd = sd(Tmin_lag8,na.rm=TRUE),
         Tmin_lag_12_sd = sd(Tmin_lag12,na.rm=TRUE),
         Precip_lag_1_sd = sd(PrecipTotal_lag1,na.rm=TRUE),
         Precip_lag_2_sd = sd(PrecipTotal_lag2,na.rm=TRUE),
         Precip_lag_3_sd = sd(PrecipTotal_lag3,na.rm=TRUE),
         Precip_lag_5_sd = sd(PrecipTotal_lag5,na.rm=TRUE),
         Precip_lag_8_sd = sd(PrecipTotal_lag8,na.rm=TRUE),
         Precip_lag_12_sd = sd(PrecipTotal_lag12,na.rm=TRUE))

Weather <- data.frame(ungroup(Weather))

Weather$Tmax_Tmin_lag1_diff <- Weather$Tmax_lag1 - Weather$Tmin_lag1
Weather$Tmax_Tmin_lag2_diff <- Weather$Tmax_lag2 - Weather$Tmin_lag2
Weather$Tmax_Tmin_lag3_diff <- Weather$Tmax_lag3 - Weather$Tmin_lag3
Weather$Tmax_Tmin_lag5_diff <- Weather$Tmax_lag5 - Weather$Tmin_lag5
Weather$Tmax_Tmin_lag8_diff <- Weather$Tmax_lag8 - Weather$Tmin_lag8
Weather$Tmax_Tmin_lag12_diff <- Weather$Tmax_lag12 - Weather$Tmin_lag12

#Weather <- 
#Weather %>%
#group_by(Station,year)%>%
#arrange(Station,Date)%>%
#mutate(Summer_Max_Avg_Year = mean(Tmax),
#Summer_Min_Avg_Year = mean(Tmin),
#Summer_Precip_Avg_Year = mean(PrecipTotal),
#Summer_Avg_Avg_Year = mean(Tavg),
#Summer_AvgSp_Avg_Year = mean(AvgSpeed))

##STANDARDIZATION AND SCALING
Weather2 <- Weather
library(caret)
trans <- preProcess(Weather[,11:ncol(Weather)], method = c("BoxCox","center", "scale"))
WeatherTrans <-  predict(trans, Weather[,11:ncol(Weather)])
Weather <- cbind(Station = Weather$Station
                 ,Date = Weather$Date
                 ,WeatherTrans)

vars_to_not_keep <- c("year","Tmax","Tmin","PrecipTotal","Tavg",
                      "DewPoint","WetBulb","Heat"
                      ,"Cool","CodeSum","StnPressure"
                      ,"SeaLevel","ResultSpeed","ResultDir","AvgSpeed")

Weather <- Weather[, !(colnames(Weather) %in% vars_to_not_keep)]

###train preprocessing
train <- merge.data.frame(train,Weather)
test <- merge.data.frame(test,Weather)
test <- test[with(test,order(Id)),]

train$month <- as.factor(substr(train$Date,6,7))
train$dayofweek<-as.factor(wday(as.Date(train$Date)))
test$month <- as.factor(substr(test$Date,6,7))
test$dayofweek<-as.factor(wday(as.Date(test$Date)))

train <- 
  train %>%
  group_by(Date,Address,Species) %>%
  mutate(record_count = length(Species))

test <- 
  test %>%
  group_by(Date,Address,Species) %>%
  mutate(record_count = length(Species))

Species_train <- with(train, data.frame(model.matrix(~Species+0)))
Species_train$SpeciesUNSPECIFIED.CULEX <- 1
Species_test <- with(test, data.frame(model.matrix(~Species+0)))

train <- cbind(train,Species_train)
test <- cbind(test,Species_test)

train$Species <- NULL
test$Species <- NULL

###############################END#########################################

spray$latitude <- as.numeric(format(spray$Latitude, digits=4))
spray$longitude <- as.numeric(format(spray$Longitude, digits=4))

train$trap_latitude <- as.numeric(format(train$Latitude, digits=4))
train$trap_longitude <- as.numeric(format(train$Longitude, digits=4))
#spray$coordinates <- paste(spray$Spray_latitude, spray$Spray_longitude, sep=",")

unique_spray <- data.frame(unique(spray[, c("latitude", "longitude")]))
unique_trap <- data.frame(unique(train[, c("trap_latitude", "trap_longitude")]))


library(sqldf)
spray_merged <- sqldf("select train.Trap,
                      train.trap_latitude, train.trap_longitude,
                      a.latitude as spray_latitude, 
                      a.longitude as spray_longitude
                      from train
                      left join unique_spray as a
                      on train.trap_latitude <= a.latitude + 0.01
                      and train.trap_latitude >= a.latitude - 0.01
                      and train.trap_longitude <= a.longitude + 0.01
                      and train.trap_longitude >= a.longitude - 0.01")

spray_merged$coordinates <- paste(spray_merged$spray_latitude, spray_merged$spray_longitude, sep=",")

library(plyr)
spray_counts <- ddply(spray_merged,~Trap,summarise,
                      spray_count = length(unique(coordinates)))            
detach(package: plyr)
spray_counts
train <- merge(train, spray_counts, by="Trap",all.x=TRUE) 
test <- merge(test, spray_counts, by="Trap",all.x=TRUE) 
test$spray_count <-ifelse(is.na(test$spray_count),1,as.numeric(test$spray_count))

train$trap_latitude <- NULL
train$trap_longitude <- NULL
##END########
