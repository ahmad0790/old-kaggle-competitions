clean_data <- function(data) {
  for(i in 1:ncol(data)){
    if(is.numeric(data[,i])){
      data[is.na(data[,i]),i] = 0
    }else{
      data[,i] = as.character(data[,i])
      data[is.na(data[,i]),i] = "MissingValue"
      data[,i] = as.factor(data[,i])
    }
  }
  return(data)
} 

library(dplyr)
library(xgboost)
setwd("/Users/ahkhan/Documents/Kaggle/Walmart/")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

train$refund <- ifelse(train$ScanCount < 0,abs(train$ScanCount),0)
train$purchase <- ifelse(train$ScanCount >= 0,abs(train$ScanCount),0)

test$refund <- ifelse(test$ScanCount < 0,abs(test$ScanCount),0)
test$purchase <- ifelse(test$ScanCount >= 0,abs(test$ScanCount),0)

Finelines_counts <- train %>%group_by(FinelineNumber)%>%summarize(finelines_count = length(VisitNumber))%>%arrange(-finelines_count)
Finelines_counts <- subset(Finelines_counts, finelines_count >50)

Upcc_counts <- train %>%group_by(Upc)%>%summarize(upc_count = length(VisitNumber))%>%arrange(-upc_count)
Upcc_counts <- subset(Upcc_counts, upc_count >50)

high_count_finline_names <- as.matrix(Finelines_counts[, "FinelineNumber"])
j=0
for (i in high_count_finline_names){
j=j+1
print(j)
train[,(ncol(train)+1)] <- ifelse(train$FinelineNumber==i,1,0)
names(train)[ncol(train)] <- paste("F_",i,sep="")
test[,(ncol(test)+1)] <- ifelse(test$FinelineNumber==i,1,0)
names(test)[ncol(test)] <- paste("F_",i,sep="")
}   

high_count_finline_names <- as.matrix(Upcc_counts[, "Upc"])
j=0
for (i in high_count_finline_names){
  j=j+1
  print(j)
  train[,(ncol(train)+1)] <- ifelse(train$Upc==i,1,0)
  names(train)[ncol(train)] <- paste("U_",i,sep="")
  test[,(ncol(test)+1)] <- ifelse(test$Upc==i,1,0)
  names(test)[ncol(test)] <- paste("U_",i,sep="")
}       

train$TripType <- as.factor(as.character(train$TripType))
factors <- with(train, data.frame(model.matrix(~DepartmentDescription + Weekday)))
train <- cbind(train, factors)
factors <- with(test, data.frame(model.matrix(~DepartmentDescription+Weekday)))
test <- cbind(test, factors)

##sum all the finelines
trips <- train %>% group_by(TripType )%>%summarize(
  Trip_Visits = length(VisitNumber))

finelines <- train %>% group_by(FinelineNumber, TripType )%>%summarize(
  Visits = sum(ScanCount))
finelines <- filter(finelines, Visits > 7)
finelines <- left_join(finelines, trips, by="TripType")

departments <- train %>% group_by(DepartmentDescription, TripType )%>%summarize(
  Visits = sum(ScanCount))
departments <- left_join(departments, trips, by="TripType")

upc <- train %>% group_by(Upc, TripType )%>%summarize(
  Visits = sum(ScanCount))
upc <- filter(upc, Visits > 7)
upc <- left_join(upc, trips, by="TripType")

#finelines$Trip_Ratio <- finelines$Visits 
finelines$Trip_Ratio <- (finelines$Visits/finelines$Trip_Visits)*100
departments$Trip_Ratio <- (departments$Visits/departments$Trip_Visits)*100
upc$Trip_Ratio <- (upc$Visits/upc$Trip_Visits)*100

upc <- upc %>% group_by(Upc)%>%summarize(
  U_TripType14 = sum(ifelse(TripType == 14, Trip_Ratio,0))
  ,U_TripType15 = sum(ifelse(TripType == 15, Trip_Ratio,0))
  ,U_TripType16 = sum(ifelse(TripType == 16, Trip_Ratio,0))
  ,U_TripType18 = sum(ifelse(TripType == 18, Trip_Ratio,0))
  ,U_TripType19 = sum(ifelse(TripType == 19, Trip_Ratio,0))
  ,U_TripType20 = sum(ifelse(TripType == 20, Trip_Ratio,0))
  ,U_TripType21 = sum(ifelse(TripType == 21, Trip_Ratio,0))
  ,U_TripType22 = sum(ifelse(TripType == 22, Trip_Ratio,0))
  ,U_TripType23 = sum(ifelse(TripType == 23, Trip_Ratio,0))
  ,U_TripType24 = sum(ifelse(TripType == 24, Trip_Ratio,0))
  ,U_TripType25 = sum(ifelse(TripType == 25, Trip_Ratio,0))
  ,U_TripType26 = sum(ifelse(TripType == 26, Trip_Ratio,0))
  ,U_TripType27 = sum(ifelse(TripType == 27, Trip_Ratio,0))
  ,U_TripType28 = sum(ifelse(TripType == 28, Trip_Ratio,0))
  ,U_TripType29 = sum(ifelse(TripType == 29, Trip_Ratio,0))
  ,U_TripType30 = sum(ifelse(TripType == 30, Trip_Ratio,0))
  ,U_TripType31 = sum(ifelse(TripType == 31, Trip_Ratio,0))
  ,U_TripType32 = sum(ifelse(TripType == 32, Trip_Ratio,0))
  ,U_TripType33 = sum(ifelse(TripType == 33, Trip_Ratio,0))
  ,U_TripType34 = sum(ifelse(TripType == 34, Trip_Ratio,0))
  ,U_TripType35 = sum(ifelse(TripType == 35, Trip_Ratio,0))
  ,U_TripType36 = sum(ifelse(TripType == 36, Trip_Ratio,0))
  ,U_TripType37 = sum(ifelse(TripType == 37, Trip_Ratio,0))
  ,U_TripType38 = sum(ifelse(TripType == 38, Trip_Ratio,0))
  ,U_TripType39 = sum(ifelse(TripType == 39, Trip_Ratio,0))
  ,U_TripType4 = sum(ifelse(TripType == 4, Trip_Ratio,0))
  ,U_TripType40 = sum(ifelse(TripType == 40, Trip_Ratio,0))
  ,U_TripType41 = sum(ifelse(TripType == 41, Trip_Ratio,0))
  ,U_TripType42 = sum(ifelse(TripType == 42, Trip_Ratio,0))
  ,U_TripType43 = sum(ifelse(TripType == 43, Trip_Ratio,0))
  ,U_TripType44 = sum(ifelse(TripType == 44, Trip_Ratio,0))
  ,U_TripType5 = sum(ifelse(TripType == 5, Trip_Ratio,0))
  ,U_TripType6 = sum(ifelse(TripType == 6, Trip_Ratio,0))
  ,U_TripType7 = sum(ifelse(TripType == 7, Trip_Ratio,0))
  ,U_TripType8 = sum(ifelse(TripType == 8, Trip_Ratio,0))
  ,U_TripType9 = sum(ifelse(TripType == 9, Trip_Ratio,0))
  ,U_TripType12 = sum(ifelse(TripType == 12, Trip_Ratio,0))
  ,U_TripType999 = sum(ifelse(TripType == 999, Trip_Ratio,0))
)

departments <- departments %>% group_by(DepartmentDescription)%>%summarize(
  D_TripType14 = sum(ifelse(TripType == 14, Trip_Ratio,0))
  ,D_TripType15 = sum(ifelse(TripType == 15, Trip_Ratio,0))
  ,D_TripType16 = sum(ifelse(TripType == 16, Trip_Ratio,0))
  ,D_TripType18 = sum(ifelse(TripType == 18, Trip_Ratio,0))
  ,D_TripType19 = sum(ifelse(TripType == 19, Trip_Ratio,0))
  ,D_TripType20 = sum(ifelse(TripType == 20, Trip_Ratio,0))
  ,D_TripType21 = sum(ifelse(TripType == 21, Trip_Ratio,0))
  ,D_TripType22 = sum(ifelse(TripType == 22, Trip_Ratio,0))
  ,D_TripType23 = sum(ifelse(TripType == 23, Trip_Ratio,0))
  ,D_TripType24 = sum(ifelse(TripType == 24, Trip_Ratio,0))
  ,D_TripType25 = sum(ifelse(TripType == 25, Trip_Ratio,0))
  ,D_TripType26 = sum(ifelse(TripType == 26, Trip_Ratio,0))
  ,D_TripType27 = sum(ifelse(TripType == 27, Trip_Ratio,0))
  ,D_TripType28 = sum(ifelse(TripType == 28, Trip_Ratio,0))
  ,D_TripType29 = sum(ifelse(TripType == 29, Trip_Ratio,0))
  ,D_TripType30 = sum(ifelse(TripType == 30, Trip_Ratio,0))
  ,D_TripType31 = sum(ifelse(TripType == 31, Trip_Ratio,0))
  ,D_TripType32 = sum(ifelse(TripType == 32, Trip_Ratio,0))
  ,D_TripType33 = sum(ifelse(TripType == 33, Trip_Ratio,0))
  ,D_TripType34 = sum(ifelse(TripType == 34, Trip_Ratio,0))
  ,D_TripType35 = sum(ifelse(TripType == 35, Trip_Ratio,0))
  ,D_TripType36 = sum(ifelse(TripType == 36, Trip_Ratio,0))
  ,D_TripType37 = sum(ifelse(TripType == 37, Trip_Ratio,0))
  ,D_TripType38 = sum(ifelse(TripType == 38, Trip_Ratio,0))
  ,D_TripType39 = sum(ifelse(TripType == 39, Trip_Ratio,0))
  ,D_TripType4 = sum(ifelse(TripType == 4, Trip_Ratio,0))
  ,D_TripType40 = sum(ifelse(TripType == 40, Trip_Ratio,0))
  ,D_TripType41 = sum(ifelse(TripType == 41, Trip_Ratio,0))
  ,D_TripType42 = sum(ifelse(TripType == 42, Trip_Ratio,0))
  ,D_TripType43 = sum(ifelse(TripType == 43, Trip_Ratio,0))
  ,D_TripType44 = sum(ifelse(TripType == 44, Trip_Ratio,0))
  ,D_TripType5 = sum(ifelse(TripType == 5, Trip_Ratio,0))
  ,D_TripType6 = sum(ifelse(TripType == 6, Trip_Ratio,0))
  ,D_TripType7 = sum(ifelse(TripType == 7, Trip_Ratio,0))
  ,D_TripType8 = sum(ifelse(TripType == 8, Trip_Ratio,0))
  ,D_TripType9 = sum(ifelse(TripType == 9, Trip_Ratio,0))
  ,D_TripType12 = sum(ifelse(TripType == 12, Trip_Ratio,0))
  ,D_TripType999 = sum(ifelse(TripType == 999, Trip_Ratio,0))
)

finelines <- finelines %>% group_by(FinelineNumber)%>%summarize(
  F_TripType14 = sum(ifelse(TripType == 14, Trip_Ratio,0))
  ,F_TripType15 = sum(ifelse(TripType == 15, Trip_Ratio,0))
  ,F_TripType16 = sum(ifelse(TripType == 16, Trip_Ratio,0))
  ,F_TripType18 = sum(ifelse(TripType == 18, Trip_Ratio,0))
  ,F_TripType19 = sum(ifelse(TripType == 19, Trip_Ratio,0))
  ,F_TripType20 = sum(ifelse(TripType == 20, Trip_Ratio,0))
  ,F_TripType21 = sum(ifelse(TripType == 21, Trip_Ratio,0))
  ,F_TripType22 = sum(ifelse(TripType == 22, Trip_Ratio,0))
  ,F_TripType23 = sum(ifelse(TripType == 23, Trip_Ratio,0))
  ,F_TripType24 = sum(ifelse(TripType == 24, Trip_Ratio,0))
  ,F_TripType25 = sum(ifelse(TripType == 25, Trip_Ratio,0))
  ,F_TripType26 = sum(ifelse(TripType == 26, Trip_Ratio,0))
  ,F_TripType27 = sum(ifelse(TripType == 27, Trip_Ratio,0))
  ,F_TripType28 = sum(ifelse(TripType == 28, Trip_Ratio,0))
  ,F_TripType29 = sum(ifelse(TripType == 29, Trip_Ratio,0))
  ,F_TripType3 = sum(ifelse(TripType == 3, Trip_Ratio,0))
  ,F_TripType30 = sum(ifelse(TripType == 30, Trip_Ratio,0))
  ,F_TripType31 = sum(ifelse(TripType == 31, Trip_Ratio,0))
  ,F_TripType32 = sum(ifelse(TripType == 32, Trip_Ratio,0))
  ,F_TripType33 = sum(ifelse(TripType == 33, Trip_Ratio,0))
  ,F_TripType34 = sum(ifelse(TripType == 34, Trip_Ratio,0))
  ,F_TripType35 = sum(ifelse(TripType == 35, Trip_Ratio,0))
  ,F_TripType36 = sum(ifelse(TripType == 36, Trip_Ratio,0))
  ,F_TripType37 = sum(ifelse(TripType == 37, Trip_Ratio,0))
  ,F_TripType38 = sum(ifelse(TripType == 38, Trip_Ratio,0))
  ,F_TripType39 = sum(ifelse(TripType == 39, Trip_Ratio,0))
  ,F_TripType4 = sum(ifelse(TripType == 4, Trip_Ratio,0))
  ,F_TripType40 = sum(ifelse(TripType == 40, Trip_Ratio,0))
  ,F_TripType41 = sum(ifelse(TripType == 41, Trip_Ratio,0))
  ,F_TripType42 = sum(ifelse(TripType == 42, Trip_Ratio,0))
  ,F_TripType43 = sum(ifelse(TripType == 43, Trip_Ratio,0))
  ,F_TripType44 = sum(ifelse(TripType == 44, Trip_Ratio,0))
  ,F_TripType5 = sum(ifelse(TripType == 5, Trip_Ratio,0))
  ,F_TripType6 = sum(ifelse(TripType == 6, Trip_Ratio,0))
  ,F_TripType7 = sum(ifelse(TripType == 7, Trip_Ratio,0))
  ,F_TripType8 = sum(ifelse(TripType == 8, Trip_Ratio,0))
  ,F_TripType9 = sum(ifelse(TripType == 9, Trip_Ratio,0))
  ,F_TripType12 = sum(ifelse(TripType == 12, Trip_Ratio,0))
  ,F_TripType999 = sum(ifelse(TripType == 999, Trip_Ratio,0))
)

train <- left_join(train, finelines, by="FinelineNumber",all.X=T)
train <- left_join(train, departments, by="DepartmentDescription",all.X=T)
train <- left_join(train, upc, by="Upc",all.X=T)

train$DepartmentDescriptionHEALTH.AND.BEAUTY.AIDS <- NULL
test <- left_join(test, finelines, by="FinelineNumber",all.X=T)
test <- left_join(test, departments, by="DepartmentDescription",all.X=T)
test <- left_join(test, upc, by="Upc",all.X=T)

visits <- train %>% group_by(VisitNumber)%>%summarize(TripType = unique(TripType) 
                                                      ,basket_item_length = length(VisitNumber)
                                                      ,distinc_finelines = length(unique(FinelineNumber))
                                                      ,distinct_departments = length(unique(DepartmentDescription))
                                                      ,distinct_upc = length(unique(Upc))
                                                      ,net_tems_bought = sum(ScanCount)
                                                      ,total_quantity_refunded = sum(refund)
                                                      ,total_quantity_purchased = sum(purchase)
                                                      ###
                                                      ,median_fineline = median(FinelineNumber)
                                                      ,median_upc = median(Upc)
                                                      #,mode_fineline = mode(FinelineNumber)
                                                      #,mode_upc = mode(Upc)
                                                      ,median_purchase = median(ScanCount)
                                                      ,max_purchase = max(ScanCount)
                                                      ,min_purchase = min(ScanCount)
                                                      
                                                      ,max_fineline = max(FinelineNumber)
                                                      ,min_fineline = min(FinelineNumber)
                                                      ,max_upc = max(Upc)
                                                      ,min_upc = min(Upc)
                                                      ,diff_fineline = max_fineline - min_fineline
                                                      ,diff_upc = max_upc - min_upc
                                                      ,diff_purchase = max_purchase - min_purchase
                                                      
                                                      ,sum_fineline = sum(FinelineNumber)
                                                      ,sum_upc = sum(Upc)
)


for(i in 11:ncol(train))
{
  print(i)
  feature <- train[,i]
  feature_data <- data.frame(cbind(VisitNumber=train$VisitNumber, features = feature))
  feature_data <- feature_data%>%group_by(VisitNumber)%>%summarize(total = sum(features))
  visits <- cbind(visits,feature_data[,"total"]) 
  names(visits)[ncol(visits)] <- names(train)[i]
  print("Column Add Complete")
}

test_visits <-  test %>% group_by(VisitNumber)%>%summarize(basket_item_length = length(VisitNumber)
                                                           ,distinc_finelines = length(unique(FinelineNumber))
                                                           ,distinct_departments = length(unique(DepartmentDescription))
                                                           ,distinct_upc = length(unique(Upc))
                                                           ,net_tems_bought = sum(ScanCount)
                                                           ,total_quantity_refunded = sum(refund)
                                                           ,total_quantity_purchased = sum(purchase)

                                                           ##
                                                           ,median_fineline = median(FinelineNumber)
                                                           ,median_upc = median(Upc)
                                                           #,mode_fineline = mode(FinelineNumber)
                                                           #,mode_upc = mode(Upc)
                                                           ,median_purchase = median(ScanCount)
                                                           ,max_purchase = max(ScanCount)
                                                           ,min_purchase = min(ScanCount)
                                                           
                                                           ,max_fineline = max(FinelineNumber)
                                                           ,min_fineline = min(FinelineNumber)
                                                           ,max_upc = max(Upc)
                                                           ,min_upc = min(Upc)
                                                           ,diff_fineline = max_fineline - min_fineline
                                                           ,diff_upc = max_upc - min_upc
                                                           ,diff_purchase = max_purchase - min_purchase
                                                           
                                                           ,sum_fineline = sum(FinelineNumber)
                                                           ,sum_upc = sum(Upc)
)

for(i in 10:ncol(test))
{
  print(i)
  feature <- test[,i]
  feature_data <- data.frame(cbind(VisitNumber=test$VisitNumber, features = feature))
  feature_data <- feature_data%>%group_by(VisitNumber)%>%summarize(total = sum(features))
  test_visits <- cbind(test_visits,feature_data[,"total"]) 
  names(test_visits)[ncol(test_visits)] <- names(test)[i]
  print("Column Add Complete")
}

train_visits <- visits
train_visits$refund_rate <- ifelse(train_visits$total_quantity_purchased==0,0, train_visits$total_quantity_refunded/train_visits$total_quantity_purchased)
test_visits$refund_rate <- ifelse(test_visits$total_quantity_purchased==0,0, test_visits$total_quantity_refunded/test_visits$total_quantity_purchased)
train_visits$DepartmentDescriptionHEALTH.AND.BEAUTY.AIDS <- NULL
#train_visits$F_550 <- NULL

rm(test)
rm(train)
rm(visits)
train_visits <- clean_data(train_visits)
test_visits <- clean_data(test_visits)

for(i in 24:3764)
{
print(i)
#train_visits[,ncol(train_visits)+1] <- train_visits[,i]/(train_visits$total_quantity_purchased+1)
train_visits[,i] <- train_visits[,i]/(train_visits$basket_item_length+1)
#names(train_visits)[ncol(train_visits)] <- paste(names(train_visits)[i],"_perc",sep="")
print("Column Add Complete")
}

for(i in 23:3763)
{
print(i)
#test_visits[,ncol(test_visits)+1] <- (test_visits[,i])/(test_visits$total_quantity_purchased+1)
test_visits[,i] <- (test_visits[,i])/(test_visits$basket_item_length+1)
#names(test_visits)[ncol(test_visits)] <- paste(names(test_visits)[i],"_perc",sep="")
print("Column Add Complete")
}

labels <- read.csv("tts.csv")
labels$TripType <- as.factor(as.character(labels$TripType))
train_visits <- left_join(train_visits, labels, by="TripType",all.x=T)

####
tlabel <- train_visits$tlabel
train_visits$tlabel <- NULL

#library(h2o)
#localH2O = h2o.init(nthread=-1, max_mem_size = "12g")
#train_visits$TripType <- as.factor(as.character(train_visits$TripType))
#train_visits$DepartmentDescriptionHEALTH.AND.BEAUTY.AIDS <- NULL
#h2o_test <- as.h2o(localH2O,test_visits)
#h2o_train <- as.h2o(localH2O,train_visits)
#target <- 2
#predictors <- c(3:(ncol(h2o_train)-1))
#train_assignments <- as.data.frame(h2o.predict(kmeans, h2o_train))
#test_assignments <- as.data.frame(h2o.predict(kmeans, h2o_test))
#train_visits$cluster <- train_assignments$predict
#test_visits$cluster <- test_assignments$predict
#Trip_Type  <- as.factor(as.character(train_visits$TripType))

###
#inTrain_sample_all<-createDataPartition(y=train_visits$tlabel,p=0.10,list=FALSE)
#train_sample<-train_visits[inTrain_sample_all,]
#valid_sample<-train_visits[-inTrain_sample_all,]
#tlabel <- train_sample$tlabel
#x = as.matrix(train_sample[,c(1,3:(ncol(train_sample)-1))])
#x = matrix(as.numeric(x),nrow(x),ncol(x))
#t = as.matrix(valid_sample[,c(3:(ncol(valid_sample)-1))])
#t = matrix(as.numeric(t),nrow(t),ncol(t))

##trainimng

x = as.matrix(train_visits[,c(1,3:(ncol(train_visits)))])
x = matrix(as.numeric(x),nrow(x),ncol(x))
rm(train_visits)
t = as.matrix(test_visits[,c(1:(ncol(test_visits)))])
t = matrix(as.numeric(t),nrow(t),ncol(t))
rm(test_visits)

param <- list("objective" = "multi:softprob"
              ,"num_class" = 38
              ,"eta"= 0.05
              ,"min_child_weight" = 5
              ,"subsample" = 0.8
              ,"colsample_bytree" = 0.8
              ,"scale_pos_weight" = 1
              ,"max_Depth" = 16
              ,"eval_metric" = "mlogloss")

##run CV test-mlogloss:1.090626+0.008413 at round 34
#cv.nround = 3
#bst.cv = xgb.cv(param=param, data = x, label = tlabel, 
                #nfold = 3, nrounds=cv.nround)
##training
nround = 500
bst1 = xgboost(param=param, data = x, label =  tlabel, nrounds=nround
               ,"eval_metric" = "mlogloss")

xgb_pred <- predict(bst1, t)
probs <- t(matrix(xgb_pred, nrow=38, ncol=length(xgb_pred)/38))
sample_submission <- read.csv("sample_submission.csv")
submission <- data.frame(cbind(VisitNumber = as.integer(sample_submission$VisitNumber), 
                               TripType_3 = probs[,1],
                               TripType_4 = probs[,2],
                               TripType_5 = probs[,3],
                               TripType_6 = probs[,4],
                               TripType_7 = probs[,5],
                               TripType_8 = probs[,6],
                               TripType_9 = probs[,7],
                               TripType_12 = probs[,8],
                               TripType_14 = probs[,9],
                               TripType_15 = probs[,10],
                               TripType_18 = probs[,11],
                               TripType_19 = probs[,12],
                               TripType_20 = probs[,13],
                               TripType_21 = probs[,14],
                               TripType_22 = probs[,15],
                               TripType_23 = probs[,16],
                               TripType_24 = probs[,17],
                               TripType_25 = probs[,18],
                               TripType_26 = probs[,19],
                               TripType_27 = probs[,20],
                               TripType_28 = probs[,21],
                               TripType_29 = probs[,22],
                               TripType_30 = probs[,23],
                               TripType_31 = probs[,24],
                               TripType_32 = probs[,25],
                               TripType_33 = probs[,26],
                               TripType_34 = probs[,37],
                               TripType_35 = probs[,28],
                               TripType_36 = probs[,29],
                               TripType_37 = probs[,30],
                               TripType_38 = probs[,31],
                               TripType_39 = probs[,32],
                               TripType_40 = probs[,33],
                               TripType_41 = probs[,34],
                               TripType_42 = probs[,35],
                               TripType_43 = probs[,36],
                               TripType_44 = probs[,37],
                               TripType_999 = probs[,38]
))
submission = format(submission, digits=2,scientific=F) # shrink the size of submission
write.csv(submission, "submission_xgboost_all_upc_005_700.csv",row.names = F)
