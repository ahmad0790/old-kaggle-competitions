setwd("/Users/ahkhan/Documents/Kaggle/Airbnb")
# load libraries
library(xgboost)
library(stringr)
library(caret)
library(car)
library(dplyr)
library(readr)

##function
create_dummy_vars <- function(data, list_values,field_val, aggregate_field_val, name_prefix) {
  j=0
  data$field <- data[,field_val]
  #data$aggregate_field <- as.character(data[,aggregate_field_val])
  for (i in list_values){
    j=j+1
    print(j)
    data[,(ncol(data)+1)] <- ifelse(data$field==i,data$secs_elapsed,0)
    names(data)[ncol(data)] <- paste(name_prefix,i,sep="")
  } 
  return(data)
}

set.seed(1)
# load data
df_train = read_csv("train_users_2.csv")
df_old = subset(df_train, date_account_created < '2014-01-01')
df_train <- subset(df_train, date_account_created>='2014-01-01')
df_test = read_csv("test_users.csv")
labels = df_train['country_destination']
df_train = df_train[-grep('country_destination', colnames(df_train))]
sessions <- read_csv("sessions.csv")

# combine train and test data
df_all = rbind(df_train,df_test)
# remove date_first_booking
df_all = df_all[-c(which(colnames(df_all) %in% c('date_first_booking')))]
# replace missing values
df_all[is.na(df_all)] <- -1

# split date_account_created in year, month and day
dac = as.data.frame(str_split_fixed(df_all$date_account_created, '-', 3))
df_all['dac_year'] = dac[,1]
df_all['dac_month'] = dac[,2]
df_all['dac_day'] = dac[,3]
df_all = df_all[,-c(which(colnames(df_all) %in% c('date_account_created')))]

# split timestamp_first_active in year, month and day
df_all[,'tfa_year'] = substring(as.character(df_all[,'timestamp_first_active']), 1, 4)
df_all['tfa_month'] = substring(as.character(df_all['timestamp_first_active']), 5, 6)
df_all['tfa_day'] = substring(as.character(df_all['timestamp_first_active']), 7, 8)
df_all = df_all[,-c(which(colnames(df_all) %in% c('timestamp_first_active')))]

# clean Age by removing values
df_all[df_all$age < 14 | df_all$age > 100,'age'] <- -1

##cleaning
ndf_browsers <- c("Apple Mail","BlackBerry Browser","Maxthon","Sogou Explorer","IE Mobile")
df_all$first_browser <-ifelse(df_all$first_browser %in% ndf_browsers,"NDF_Browser",as.character(df_all$first_browser))
pop_browsers <- df_all %>% group_by(first_browser) %>% summarize(count = length(first_browser)) %>% arrange(-count)%>%filter(count>=100)
pop_browsers <- as.matrix(pop_browsers[,"first_browser"])
df_all$first_browser <-ifelse(df_all$first_browser %in% pop_browsers,as.character(df_all$first_browser),"Other")

df_all$signup_flow <-ifelse(df_all$signup_flow == 21,0,df_all$signup_flow)

pop_lang <- df_all %>% group_by(language) %>% summarize(count = length(language)) %>% arrange(-count)%>%filter(count>=100)
pop_lang <- as.matrix(pop_lang[,"language"])
df_all$language <-ifelse(df_all$language %in% pop_lang,as.character(df_all$language),"Other")

pop_provider <- df_all %>% group_by(affiliate_provider) %>% summarize(count = length(language)) %>% arrange(-count)%>%filter(count>=50)
pop_provider <- as.matrix(pop_provider[,"affiliate_provider"])
df_all$affiliate_provider <-ifelse(df_all$affiliate_provider %in% pop_provider,as.character(df_all$affiliate_provider),"Other")

df_all$signup_method <-ifelse(df_all$signup_method=="weibo","google",as.character(df_all$signup_method))
df_all$signup_flow <-ifelse(df_all$signup_flow==14,0,(df_all$signup_flow))
df_all$first_affiliate_tracked <-ifelse(df_all$first_affiliate_tracked=="local ops","product",as.character(df_all$first_affiliate_tracked))

# one-hot-encoding features
ohe_feats = c('gender', 'signup_method', 'signup_flow', 'language', 'affiliate_channel', 'affiliate_provider', 'first_affiliate_tracked', 'signup_app', 'first_device_type', 'first_browser')
dummies <- dummyVars(~ gender + signup_method + signup_flow + language + affiliate_channel + affiliate_provider + first_affiliate_tracked + signup_app + first_device_type + first_browser, data = df_all)
df_all_ohe <- as.data.frame(predict(dummies, newdata = df_all))
df_all_combined <- cbind(df_all[,-c(which(colnames(df_all) %in% ohe_feats))],df_all_ohe)

###aggregate user data for sessions
high_action_counts <- sessions %>% group_by(action) %>% summarize(action_count = length(user_id)) %>% arrange(-action_count)%>%filter(action_count>=50)
high_action_counts <- as.matrix(high_action_counts[,"action"])

high_action_type_counts <- sessions %>% group_by(action_type) %>% summarize(action_count = length(user_id)) %>% arrange(-action_count)%>%filter(action_count>=50)
high_action_type_counts <- as.matrix(high_action_type_counts[,"action_type"])

high_action_detail_counts <- sessions %>% group_by(action_detail) %>% summarize(action_count = length(user_id)) %>% arrange(-action_count)%>%filter(action_count>=50)
high_action_detail_counts <- as.matrix(high_action_detail_counts[,"action_detail"])

high_device_counts <- sessions %>% group_by(device_type) %>% summarize(action_count = length(user_id)) %>% arrange(-action_count)%>%filter(action_count>=50)
high_device_counts <- as.matrix(high_device_counts[,"device_type"])

sessions <- create_dummy_vars(sessions,high_action_counts,"action","secs_elapsed","A_")
sessions <- create_dummy_vars(sessions,high_action_type_counts,"action_type","secs_elapsed","AT_")
sessions <- create_dummy_vars(sessions,high_action_detail_counts,"action_detail","secs_elapsed","AD_")
sessions <- create_dummy_vars(sessions,high_device_counts,"device_type","secs_elapsed","D_")

sessions_agg <- sessions[,c(1,8:ncol(sessions))] %>% group_by(user_id) %>% summarise_each(funs(sum))
sessions_agg <- data.frame(sessions_agg)

sessions_agg_mean <- sessions[,c(1,8:ncol(sessions))] %>% group_by(user_id) %>% summarise_each(funs(mean))
for (i in 2:ncol(sessions_agg_mean)){
names(sessions_agg_mean)[i] <- paste(names(sessions_agg_mean)[i],"_mean",sep="")
}
sessions_agg_sd <- sessions[,c(1,8:ncol(sessions))] %>% group_by(user_id) %>% summarise_each(funs(sd))
for (i in 2:ncol(sessions_agg_sd)){
  names(sessions_agg_sd)[i] <- paste(names(sessions_agg_sd)[i],"_sd",sep="")
}
sessions_agg_mean <- data.frame(sessions_agg_mean)
sessions_agg_sd <- data.frame(sessions_agg_sd)

sessions_counts <- sessions %>% group_by(user_id) %>% summarise(total_action = length(action)
                                                                ,unique_actions = length(unique(action))
                                                                ,unique_action_type = length(unique(action_type))
                                                                ,unique_action_detail = length(unique(action_detail))
                                                                ,unique_device_type = length(unique(device_type))
                                                                ,mean_secs_elapsed =  mean(secs_elapsed, na.rm=T)
                                                                ,sd_secs_elapsed = sd(secs_elapsed, na.rm=T)
                                                                ,tot_secs_elapsed = sum(secs_elapsed, na.rm=T)
                                                                )
                                                                
###
sessions_all <- cbind(sessions_agg, sessions_agg_mean[,2:ncol(sessions_agg_mean)], sessions_agg_sd[,2:ncol(sessions_agg_sd)])
df_all_combined <- left_join(df_all_combined, sessions_counts, by=c("id"="user_id"),all.x=T)
df_all_combined <- left_join(df_all_combined, sessions_all, by=c("id"="user_id"),all.x=T)
df_all_combined[is.na(df_all_combined)] <- -1

# split train and test
X = df_all_combined[df_all_combined$id %in% df_train$id,]
y <- recode(labels$country_destination,"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11")
X_test = df_all_combined[df_all_combined$id %in% df_test$id,]

# train xgboost 0.291973
xgb_cv <- xgb.cv(data = data.matrix(X[,-c(1,6)]), 
               label = y, 
               eta = 0.05,
               max_depth = 9, 
               nround=30, 
               subsample = 0.8,
               colsample_bytree = 0.8,
               seed = 1,
               #min_child_weight = 7,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 12,
               nthread = 4,
               nfold=5)

# predict values in test set
y_pred <- predict(xgb_cv, data.matrix(X_test[,-c(1,6)]))

# extract the 5 classes with highest probabilities
predictions <- as.data.frame(matrix(y_pred, nrow=12))
rownames(predictions) <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')
predictions_top5 <- as.vector(apply(predictions, 2, function(x) names(sort(x)[12:8])))

# create submission 
ids <- NULL
for (i in 1:NROW(X_test)) {
  idx <- X_test$id[i]
  ids <- append(ids, rep(idx,5))
}
submission <- NULL
submission$id <- ids
submission$country <- predictions_top5

# generate submission file
submission <- as.data.frame(submission)
write.csv(submission, "submission_final_v3.csv", quote=FALSE, row.names = FALSE)
