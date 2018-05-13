#detach(package: h2o)
#library(h2o)
#library(Metrics)
#localH2O = h2o.init(nthread=-1, max_mem_size = "8g")

predictors <- c(4:11,13:(ncol(train)))
response = 12
train$year <- as.character(substr(train$Date,1,4))
test$year <- as.factor(substr(test$Date,1,4))

train_2011_13 <- train[which(train$year == '2013' | train$year == '2011'),]
train_2011_13$year <- NULL
auc <- c(0,0,0,0)
i = 1 
test_2013 <- train[which(train$year == '2013'),]
test_2011 <- train[which(train$year == '2011'),]
test_2007 <- train[which(train$year == '2007'),]
test_2009 <- train[which(train$year == '2009'),]

train_2013 <- train[which(train$year != '2013'),]
train_2011 <- train[which(train$year != '2011'),]
train_2007 <- train[which(train$year != '2007'),]
train_2009 <- train[which(train$year != '2009'),]

years <- c("2013","2011","2009","2007")

for (year in years){
  print(i)
  if (year == '2013'){
    train_2013$year <- NULL
    test_2013$year <- NULL
    train.hex <- as.h2o(localH2O,train_2013)
    test.hex <- as.h2o(localH2O,test_2013)
  }
    
  if (year == '2011'){
    train_2013$year <- NULL
    test_2013$year <- NULL
    train.hex <- as.h2o(localH2O,train_2011)
    test.hex <- as.h2o(localH2O,test_2011)
  }
  
  if (year == '2009'){
    train_2009$year <- NULL
    test_2009$year <- NULL
    train.hex <- as.h2o(localH2O,train_2009)
    test.hex <- as.h2o(localH2O,test_2009)
  }
  
  if (year == '2007'){
    train_2007$year <- NULL
    test_2007$year <- NULL
    train.hex <- as.h2o(localH2O,train_2007)
    test.hex <- as.h2o(localH2O,test_2007)
  }
  
  print("starting model")
  model <- h2o.randomForest(x=predictors,y=response,data = train.hex,
                            classification = T,ntree = 200
                            ,importance=TRUE
                            ,seed = 5650987299948753920)
  
  pred <- h2o.predict(model,test.hex)
  p <- as.data.frame(pred)
  actuals <- as.data.frame(test.hex$WnvPresent)
  auc[i] <-  auc(actuals$WnvPresent,p[,3])
  i = i + 1
  print(i)
  average_auc <- mean(auc)
}
train$year <- NULL 
test$year <- NULL
auc
average_auc

#0.723744, 0.7284819, 0.7370093 with sd added, 0.7476121 0.83 with Mosq added and sd added 
#improvement over 0.80 whcih is only num mosq, 0.8063478

##ALL DATA
#library(plyr)
#traps <- ddply(train,~Trap,summarise,
               #trap_sum_wnv=sum(as.numeric(as.character(WnvPresent))))

#detach(package: plyr)

#traps <- arrange(traps, -trap_sum_wnv )
#zero_traps <- subset(traps,trap_sum_wnv==0 )
#zero_traps <- zero_traps[,1]
#zero_traps <- paste(zero_traps,sep="",collapse="|")

#train_2007$zero_trap_ind <- grepl(zero_traps,train_2007$Trap)
#train_2007 <- subset(train_2007, zero_trap_ind ==0)
#train_2007$zero_trap_ind <- NULL

test.hex <- as.h2o(localH2O,test)
train.hex <- as.h2o(localH2O,train_2007)

predictors <- c(4:11,13:ncol(train.hex))
response = 12
# seed current position -5497046075401094144 gives a score of 0.786
model_all <- h2o.randomForest(x=predictors,y=response,data = train.hex,
                          #sample.rate = 0.5,
                          classification = T
                          ,ntree = 501
                          #,importance=TRUE
                          #,nfolds=5
                          ,seed = 5650987299948753920
                          )

model_all@model$auc

rf.VI = data.frame(t(model_all@model$varimp))
write.csv(rf.VI, "rf_imp_2.csv")
pred <- h2o.predict(model_all,test.hex)
p <- as.data.frame(pred)

#summary(p)
#subm[,2] = p[,3]
#summary(subm)
#write.csv(subm,file=paste0(path,"submisison_rf_mosqz-sd.csv"),row.names=FALSE)

##removing unnecessary species
test$pred <- p[,3]
test$pred <- ifelse(test$SpeciesCULEX.ERRATICUS == 1,0, as.numeric(test$pred))
test$pred <- ifelse(test$SpeciesCULEX.SALINARIUS == 1,0, as.numeric(test$pred))
test$pred <- ifelse(test$SpeciesCULEX.TARSALIS == 1,0, as.numeric(test$pred))
test$pred <- ifelse(test$SpeciesCULEX.TERRITANS == 1,0, as.numeric(test$pred))
test$pred <- ifelse(test$SpeciesUNSPECIFIED.CULEX == 1,0, as.numeric(test$pred))

#test$zero_trap_ind <- grepl(zero_traps,test$Trap)
#test$pred <- ifelse(test$zero_trap_ind == 1,0, as.numeric(test$pred))
#test$zero_trap_ind <- NULL

p <- test$pred
subm[,2] = p
summary(subm)
write.csv(subm,file=paste0(path,"submisison_rf_mosqz-sd_no_2007_3.csv"),row.names=FALSE)

#BEST AUC with seed: 0.8034106, with sd 0.8050112, current best nfold 0.7786916
##AUC says improved to 0.8306806 with spray coutn added, 0.8536202 w 07 removed
##clean both num mosquitos and pred for better scores
#test$pred <- NULL
# lasso scores 0.7898
