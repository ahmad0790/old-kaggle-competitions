test.hex <- as.h2o(localH2O,test)
train.hex <- as.h2o(localH2O,train)
predictors <- c(4:10,13:(ncol(train)))
response = 11
num_mosq <- h2o.randomForest(x=predictors,y=response,data = train.hex,
classification = F,ntree = 500
#,importance=TRUE
,type="BigData"
#,nfolds=5
,seed = 5650987299948753920)

num_mosq
pred_mosq <- h2o.predict(num_mosq,test.hex)
pred_mosq <- as.data.frame(pred_mosq)
summary(pred_mosq)
test$NumMosquitos <- pred_mosq[,1]
##removing unncessary mosquitos
test$NumMosquitos <- ifelse(test$SpeciesCULEX.ERRATICUS == 1,1, as.numeric(test$NumMosquitos))
test$NumMosquitos <- ifelse(test$SpeciesCULEX.SALINARIUS == 1,1, as.numeric(test$NumMosquitos))
test$NumMosquitos <- ifelse(test$SpeciesCULEX.TARSALIS == 1,0, as.numeric(test$NumMosquitos))
test$NumMosquitos <- ifelse(test$SpeciesCULEX.TERRITANS == 1,0, as.numeric(test$NumMosquitos))
test$NumMosquitos <- ifelse(test$SpeciesUNSPECIFIED.CULEX == 1,0, as.numeric(test$NumMosquitos))
