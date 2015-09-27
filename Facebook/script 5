library(data.table)
library(sqldf)
require(bit64)
library(dplyr)

setwd("~/Documents/Kaggle/Facebook")

bids <- fread("bids.csv")
train <- fread("train.csv",stringsAsFactors=TRUE)
train$train_ind <- 1
test <- fread("test.csv")
test$train_ind <- 0
test$outcome <- NA
merged<- rbind(train,test)
merged$row_name <- rownames(merged)
merged <- merge(bids, merged, by = "bidder_id", all = TRUE)

#merged <- arrange(merged[which(train_ind==1),],bidder_id,auction,time)

merged <- 
  merged %>%
  group_by(bidder_id, auction) %>%
  mutate(max_bid_id_tm = max(time))

merged <- 
  merged %>%
  group_by(bidder_id, auction) %>%
  mutate(min_bid_id_tm = min(time))

merged$bidder_age <- (merged$max_bid_id_tm - merged$min_bid_id_tm)/100000000

merged <- 
  merged %>%
  group_by(auction) %>%
  mutate(max_all_id_tm = max(time))

merged$auction_time_diff <- (merged$max_all_id_tm - merged$max_bid_id_tm)/100000000

##TIME AVERAGES
merged$time <- as.POSIXct((merged$time)/100000000, origin = "2001-04-01")

merged$hour <- hour(merged$time)
merged$weekday <- factor(wday(merged$time))
merged$month <- month(merged$time)

merged$time_char <- as.character(merged$time)
merged$min <- as.numeric(substr(merged$time_char, 15, 16))
merged$sec <- as.numeric(substr(merged$time_char, 18, 19))
merged$date <- as.Date(merged$time)

merged$time_char <- NULL

merged <- 
  merged %>%
  group_by(bidder_id, auction) %>%
  mutate(bid_id_cnt = length(bid_id))

merged <- 
  merged %>%
  group_by(auction) %>%
  mutate(all_id_cnt = length(bid_id))

merged$bid_ratio <- merged$bid_id_cnt/merged$all_id_cnt

merged <- 
  merged %>%
  group_by(bidder_id,auction) %>%
  arrange(bidder_id,auction,time)%>%
  mutate(lag_country = as.numeric(ifelse(country==lag(country),0,1))
         ,lag_url = as.numeric(ifelse(lag(url) == url,0,1))
         ,lag_ip = as.numeric(ifelse(lag(ip) == ip,0,1))
         ,lag_device = as.numeric(ifelse(lag(device)==device,0,1))) 

merged <- 
  merged %>%
  group_by(bidder_id,date,hour,min) %>%
  mutate(avg_min_bids = mean(length(bid_id)))

merged <- 
  merged %>%
  group_by(bidder_id,date,hour,min,sec) %>%
  mutate(avg_sec_bids = mean(length(bid_id)))

merged <- 
  merged %>%
  group_by(bidder_id,date,hour) %>%
  mutate(avg_hr_bids = mean(length(bid_id)))

merged <- 
  merged %>%
  group_by(bidder_id,date) %>%
  mutate(avg_daily_bids = mean(length(bid_id)))

merged <- 
  merged %>%
  group_by(bidder_id,date,hour,min) %>%
  mutate(num_countries = length(unique(country))
         ,num_auctions = length(unique(auction))
         ,num_ip = length(unique(ip))
         ,num_url = length(unique(url))
         #,num_merchandising = length(unique(merchandise))
         ,num_device = length(unique(device)))

merged <- 
  merged %>%
  group_by(bidder_id) %>%
  mutate(auction_counts = length(unique(auction)),
         device_counts = length(unique(device)),
         country_counts = length(unique(country)),
         ip_counts = length(unique(ip)),
         url_counts = length(unique(url)))


###AGGREGATING
aggregated <- sqldf("select bidder_id,outcome,train_ind,
                    sum(lag_device) as lag_device,
                    sum(lag_url) as lag_url,
                    sum(lag_ip) as lag_ip,
                    sum(lag_country) as lag_country,
                    
                    avg(bid_ratio) as avg_bid_ratio,
                    max(bid_ratio) as max_bid_ratio,
                    stdev(bid_ratio) as std_bid_ratio,
                    
                    avg(auction_time_diff) as avg_auc_time_diff,
                    min(auction_time_diff) as max_auc_time_diff,
                    
                    avg(bidder_age) as avg_bidder_age,

                    max(avg_min_bids) as max_min_bids,
                    max(avg_sec_bids) as max_sec_bids,
                    max(avg_daily_bids) as max_daily_bids,
                    max(avg_hr_bids) as max_hr_bids,
                    
                    stdev(avg_min_bids) as sd_min_bids,
                    stdev(avg_sec_bids) as sd_sec_bids,
                    stdev(avg_daily_bids) as sd_daily_bids,
                    stdev(avg_hr_bids) as sd_hr_bids,
                    
                    avg(avg_min_bids) as avg_min,
                    avg(avg_sec_bids) as avg_sec,
                    avg(avg_daily_bids) as avg_day,
                    avg(avg_hr_bids) as avg_hr,
                    
                    avg(num_auctions) as avg_num_auctions,
                    avg(num_countries) as avg_num_countries,
                    avg(num_ip) as avg_num_ip,
                    avg(num_url) as avg_num_url,
                    avg(num_device) as avg_num_device,
                    
                    count(bid_id) as bids,
                    count(distinct auction) as auction_counts, 
                    count(distinct device) as device_counts,
                    count(distinct country) as country_counts,
                    count(distinct ip) as ip_counts,
                    count(distinct url) as url_counts
                    
                    from merged
                    group by bidder_id,outcome,train_ind")

aggregated2 <- aggregated
#AGGREGATED IS THE FINAL BASE 

aggregated$lag_device <- ifelse(is.na(aggregated$lag_device),0,as.numeric(aggregated$lag_device))
aggregated$lag_country <- ifelse(is.na(aggregated$lag_country),0,as.numeric(aggregated$lag_country))
aggregated$lag_ip <- ifelse(is.na(aggregated$lag_ip),0,as.numeric(aggregated$lag_ip))
aggregated$lag_url <- ifelse(is.na(aggregated$lag_url),0,as.numeric(aggregated$lag_url))



aggregated_train <- as.data.frame(subset(aggregated, aggregated$train_ind == 1))
aggregated_test <- as.data.frame(subset(aggregated, aggregated$train_ind == 0))

#inTrain_sample_all<-createDataPartition(y=aggregated_train$outcome,p=0.70,list=FALSE)
#aggregated_train<-aggregated_train[inTrain_sample_all,]
#aggregated_test<-aggregated_train[-inTrain_sample_all,]

aggregated_train$bidder_id <- NULL
#aggregated_test$bidder_id <- NULL
aggregated_train$train_ind <- NULL
aggregated_test$train_ind <- NULL
aggregated_test$outcome <- NULL
aggregated_train$outcome <- as.factor(aggregated_train$outcome)
#aggregated_test$outcome <- as.factor(aggregated_test$outcome)


#aggregated_train$bids_country <- aggregated_train$bids*aggregated_train$country_counts
#aggregated_train$bids_auction <- aggregated_train$bids*aggregated_train$auction_counts
#aggregated_train$bids_ip <- aggregated_train$bids*aggregated_train$ip_counts
#aggregated_train$bids_url <- aggregated_train$bids*aggregated_train$url_counts
#aggregated_train$bids_device <- aggregated_train$bids*aggregated_train$device_counts

library(randomForest)
rf <- randomForest(outcome~., data=aggregated_train, ntree=201,importance=TRUE,na.action=na.omit)
rf_predict <- predict(rf,newdata=aggregated_test, type="prob")
rf_predict <- data.frame(prediction = rf_predict[,2])


#library(ROCR)
predROCR = prediction(rf_predict, aggregated_test$outcome)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values

rf_imp <- data.frame(row_name = rownames(importance(rf)),importance(rf))
arrange(rf_imp, -MeanDecreaseAccuracy)

###SUBMISSION
submission_test<- data.frame(cbind(bidder_id = aggregated_test$bidder_id, prediction = rf_predict$prediction))
write.csv(submission_test,"submission4.csv",row.names=FALSE)

#####END########
