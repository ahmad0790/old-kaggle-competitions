library(data.table)
library(sqldf)
library(plyr)

setwd("~/Documents/Kaggle/Facebook")
bids <- fread("bids.csv")

train <- fread("train.csv",stringsAsFactors=TRUE)
train$train_ind <- 1
train_merged <- merge(bids, train, by = "bidder_id", all.y = TRUE)

test <- fread("test.csv")
test$train_ind <- 0
test$outcome<-NA
test_merged <- merge(bids, test, by = "bidder_id", all.y = TRUE)

merged<- rbind(train_merged,test_merged)
merged$row_name <- rownames(merged)

##COUNTRY
country <- ddply(train_merged,~country,summarise,bots=sum(outcome),freq=length(bid_id),bot_ratio=as.numeric(bots/freq*100))
country <- arrange(country,-bot_ratio)
merged$risk_country <- as.factor(grepl(paste(country[1:(0.1*nrow(country)),"country"],sep="",collapse="|"),merged$country))
merged$risk_country_20_40 <- as.factor(grepl(paste(country[21:40,"country"],sep="",collapse="|"),merged$country))
merged$risk_country_40_100 <- as.factor(grepl(paste(country[41:100,"country"],sep="",collapse="|"),merged$country))

##MERCHANDISE
merchandise<- ddply(train_merged,~merchandise,summarise,bots=sum(outcome),freq=length(bid_id),bot_ratio=as.numeric(bots/freq*100))
merchandise <- arrange(merchandise,-bot_ratio)
merged$computers <- factor(grepl("computers",merged$merchandise))
merged$sporting_goods <- factor(grepl("sporting goods",merged$merchandise))
merged$jewelry <- as.factor(grepl("jewelry",merged$merchandise))
merged$mobile <- as.factor(grepl("mobile",merged$merchandise))
merged$booksandmusic <- (factor(grepl("books and music",merged$merchandise)))

device <- ddply(train_merged,~device,summarise,bots=sum(outcome),freq=length(bid_id),bot_ratio=as.numeric(bots/freq*100))
device <- arrange(device,-bot_ratio)
device_list_0_10 <- paste("\\<",device[1:(0.1*nrow(device)),"device"],"\\>",sep="",collapse="|")
device_list_10_20 <- paste("\\<",device[(0.1*nrow(device)+1):(0.2*nrow(device)),"device"],"\\>",sep="",collapse="|")
device_list_20_40 <- paste("\\<",device[((0.2*nrow(device))+1):(0.4*nrow(device)),"device"],"\\>",sep="",collapse="|")
device_list_40_60 <- paste("\\<",device[((0.4*nrow(device))+1):(0.6*nrow(device)),"device"],"\\>",sep="",collapse="|")
device_list_60_80 <- paste("\\<",device[((0.6*nrow(device))+1):(0.8*nrow(device)),"device"],"\\>",sep="",collapse="|")

merged$risk_device_10 <- as.factor(grepl(device_list_0_10,merged$device))
merged$risk_device_20 <- as.factor(grepl(device_list_10_20,merged$device))
merged$risk_device_40 <- as.factor(grepl(device_list_20_40,merged$device))
merged$risk_device_60 <- as.factor(grepl(device_list_40_60,merged$device))
merged$risk_device_80 <- as.factor(grepl(device_list_60_80,merged$device))

detach(package:plyr)
require(bit64)
library(dplyr)

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
#merged$weekday <- factor(wday(merged$time))
#merged$month <- month(merged$time)

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

#merged <- 
  #merged %>%
  #group_by(bidder_id,auction) %>%
  #arrange(bidder_id,auction,time)%>%
  #mutate(lag_country = as.numeric(ifelse(country==lag(country),0,1))
         #,lag_url = as.numeric(ifelse(lag(url) == url,0,1))
         #,lag_ip = as.numeric(ifelse(lag(ip) == ip,0,1))
         #,lag_device = as.numeric(ifelse(lag(device)==device,0,1))) 

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

merged <- fread("merged.csv")
###AGGREGATING
aggregated <- sqldf("select bidder_id,outcome,train_ind,
                    
                    sum(case when booksandmusic = 'TRUE' THEN 1 ELSE 0 END) as sum_books,
                    sum(case when sporting_goods = 'TRUE' THEN 1 ELSE 0 END) as sum_sporting_goods,
                    sum(case when jewelry = 'TRUE' THEN 1 ELSE 0 END) as sum_jewelry,
                    sum(case when mobile = 'TRUE' THEN 1 ELSE 0 END) as sum_mobile,
                    sum(case when computers = 'TRUE' THEN 1 ELSE 0 END) as sum_computers,

                    sum(case when risk_country = 'TRUE' THEN 1 ELSE 0 END) as sum_risk_country,
                    sum(case when risk_country_20_40 = 'TRUE' THEN 1 ELSE 0 END) as risk_country_20_40,
                    
                    sum(case when risk_device_10 = 'TRUE' or risk_device_20 = 'TRUE' THEN 1 ELSE 0 END) as risk_device_20,
                    sum(case when risk_device_40 = 'TRUE' THEN 1 ELSE 0 END) as risk_device_40,

                    sum(lag_device) as lag_device,
                    sum(lag_ip) as lag_ip,
                    sum(lag_country) as lag_country,
                    
                    avg(bid_ratio) as avg_bid_ratio,
                    
                    avg(auction_time_diff) as avg_auc_time_diff,
                    
                    avg(bidder_age) as avg_bidder_age,
                    
                    max(avg_min_bids) as max_min_bids,
                    max(avg_daily_bids) as max_daily_bids,
                    max(avg_hr_bids) as max_hr_bids,

                    min(avg_min_bids) as min_min_bids,
                    min(avg_daily_bids) as min_daily_bids,
                    min(avg_hr_bids) as min_hr_bids,
                    
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
aggregated$bids <- ifelse(aggregated$bids==0,1,as.numeric(aggregated$bids))

aggregated$lag_device <- ifelse(is.na(aggregated$lag_device),0,as.numeric(aggregated$lag_device))
aggregated$lag_country <- ifelse(is.na(aggregated$lag_country),0,as.numeric(aggregated$lag_country))
aggregated$lag_ip <- ifelse(is.na(aggregated$lag_ip),0,as.numeric(aggregated$lag_ip))

aggregated$sum_books <- ifelse(is.na(aggregated$sum_books),0,as.numeric(aggregated$sum_books/aggregated$bids)) 
aggregated$sum_sporting_goods <- ifelse(is.na(aggregated$sum_sporting_goods),0,as.numeric(aggregated$sum_sporting_goods/aggregated$bids)) 
aggregated$sum_jewelry <- ifelse(is.na(aggregated$sum_jewelry),0,as.numeric(aggregated$sum_jewelry/aggregated$bids)) 
aggregated$sum_mobile <- ifelse(is.na(aggregated$sum_mobile),0,as.numeric(aggregated$sum_mobile/aggregated$bids)) 
aggregated$sum_computers <- ifelse(is.na(aggregated$sum_computers),0,as.numeric(aggregated$sum_computers/aggregated$bids)) 

aggregated$sum_risk_country <- as.numeric(aggregated$sum_risk_country) /aggregated$bids 
aggregated$risk_country_20_40 <- as.numeric(aggregated$risk_country_20_40) /aggregated$bids 
#aggregated$risk_country_40_100 <- as.numeric(aggregated$risk_country_40_100)/aggregated$bids 

#aggregated$risk_device_10 <- as.numeric(aggregated$risk_device_10)/aggregated$bids 

aggregated$risk_device_20 <- as.numeric(aggregated$risk_device_20) /aggregated$bids 
aggregated$risk_device_40 <- as.numeric(aggregated$risk_device_40) /aggregated$bids 

aggregated$risk_device_60 <- as.numeric(aggregated$risk_device_60)/aggregated$bids 
aggregated$risk_device_80 <- as.numeric(aggregated$risk_device_80)/aggregated$bids 
aggregated$risk_device <- as.numeric(aggregated$risk_device_10)+as.numeric(aggregated$risk_device_20)+as.numeric(aggregated$risk_device_60) 

aggregated$sum_num_all <- aggregated$auction_counts+aggregated$device_counts+aggregated$country_counts+aggregated$ip_counts+aggregated$url_counts
aggregated$num_avg <- aggregated$avg_num_auctions+aggregated$avg_num_countries+aggregated$avg_num_ip+aggregated$avg_num_url+aggregated$avg_num_device

#for (i in 4:ncol(aggregated)){
  #aggregated[,i] <- scale(aggregated[,i])
#}

trans <- preProcess(aggregated[,4:ncol(aggregated)], method = c("BoxCox", "center", "scale"))
transformedAggregated <-  predict(trans, aggregated[,4:ncol(aggregated)])
aggregrated_tran <- cbind(bidder_id = aggregated$bidder_id
                     ,outcome = aggregated$outcome
                     ,train_ind = aggregated$train_ind
                     ,transformedAggregated)

aggregated_train <- as.data.frame(subset(aggregrated_tran, aggregrated_tran$train_ind == 1))
aggregated_test <- as.data.frame(subset(aggregrated_tran, aggregrated_tran$train_ind == 0))


aggregated_train <- as.data.frame(subset(aggregated, aggregated$train_ind == 1))
aggregated_test <- as.data.frame(subset(aggregated, aggregated$train_ind == 0))

#inTrain_sample_all<-createDataPartition(y=aggregated_train$outcome,p=0.70,list=FALSE)
#aggregated_train<-aggregated_train[inTrain_sample_all,]
#aggregated_test<-aggregated_train[-inTrain_sample_all,]

aggregated_train$bidder_id <- NULL
aggregated_train$train_ind <- NULL
aggregated_test$train_ind <- NULL
aggregated_test$outcome <- NULL
aggregated_train$outcome <- as.factor(aggregated_train$outcome)


#aggregated_train$bids_country <- aggregated_train$bids*aggregated_train$country_counts
#aggregated_train$bids_auction <- aggregated_train$bids*aggregated_train$auction_counts
#aggregated_train$bids_ip <- aggregated_train$bids*aggregated_train$ip_counts
#aggregated_train$bids_url <- aggregated_train$bids*aggregated_train$url_counts
#aggregated_train$bids_device <- aggregated_train$bids*aggregated_train$device_counts

library(randomForest)
rf <- randomForest(outcome~., data=aggregated_train, ntree=201,importance=TRUE)
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
submission_test<- data.frame(cbind(bidder_id = as.character(aggregated_test$bidder_id), prediction = rf_predict$prediction))
write.csv(submission_test,"submission_rf_3.csv",row.names=FALSE)

#####END########

library(AUCRF)
fit<-AUCRF(outcome~.,data=aggregated_train,pdel=0)
vars_to_keep <- c("bidder_id","outcome","train_ind","sum_risk_device","device_counts"
                  ,"avg_hr","max_hr_bids"
                  ,"avg_day","max_daily_bids","avg_auc_time_diff"
                  ,"sd_hr_bids","bids","avg_bid_ratio","avg_min"
                  ,"avg_bidder_age")

aggregated <- aggregated[,vars_to_keep]

rf_cv <- rfcv(aggregated_train[4:ncol(aggregated_train)], aggregated_train[,2]
              , cv.fold=5)


url <- ddply(train_merged,~url,summarise,bots=sum(outcome),freq=length(bid_id),bot_ratio=as.numeric(bots/freq*100))
url <- arrange(url,-bot_ratio)

ip <- ddply(train_merged,~ip,summarise,bots=sum(outcome),freq=length(bid_id),bot_ratio=as.numeric(bots/freq*100))
ip <- arrange(ip,-bot_ratio)

risk_device_40+
  sum_risk_device+
  ++
  avg_day

rf <- randomForest(outcome~
                     avg_day+
                     max_hr_bids+
                     sd_hr_bids+
                     avg_auc_time_diff+
                     device_counts+
                     avg_min+
                     avg_bid_ratio+
                     num_avg+
                     avg_bidder_age+
                     sum_num_all+
                     risk_device+
                     risk_country+
                     avg_sec
                   ,data=aggregated_train,importance=TRUE,ntree=201
                   ,nodesize=5)
