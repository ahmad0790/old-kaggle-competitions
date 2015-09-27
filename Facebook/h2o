log_model <- glm(outcome~.,data=aggregated_train,family="binomial")
log_predict <- predict(log_model,newdata=aggregated_test,type="response")

predROCR = prediction(log_predict, aggregated_test$outcome)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values

average_predict <- 0.4*log_predict + 3.6*rf_predict

predROCR = prediction(average_predict, aggregated_test$outcome)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values


if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download, install and initialize the H2O package for R.
install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/rel-kahan/5/R", getOption("repos"))))

library(h2o)
localH2O = h2o.init()
localH2O = h2o.init(nthread=8)

predictors <- 2:(ncol(aggregated_train))
response <- 1

train_filtered.hex <- as.h2o(localH2O,aggregated_train)
test_filtered.hex <- as.h2o(localH2O,aggregated_test)

train_filtered.hex <- as.h2o(localH2O,scaledTraindata)
test_filtered.hex <- as.h2o(localH2O,scaledTestdata)

randomforest <- h2o.randomForest(x = predictors, y = as.numeric(response), data = train_filtered.hex, 
                                 classification = TRUE
                                 ,importance=TRUE
                                 ,ntree =201
                                 ,depth=20
                                 ,type="BigData"
                                 ,nfolds=5)
randomforest@model$auc
nn_model@model$auc


predictions_rf <- as.data.frame(h2o.predict(randomforest, test_filtered.hex ))
predictions <- cbind(bidder_id = aggregated_test$bidder_id,prediction = predictions_rf[,3])
write.csv(predictions,"rf_pred_h2o.csv",row.names=FALSE)


rf.VI = (data.frame(t(randomforest@model$varimp),row.names=TRUE)
arrange(rf.VI,-Relative.importance)
write.csv(rf.VI, "var_imps.csv")
print(j)

model_key n.trees interaction.depth n.minobsinnode n.bins shrinkage
1   GBM_855a47b202f8c6e5e2307ac1e79e489b      50                 7             20     20       0.2

gbm <- h2o.gbm(x = predictors,
                              y = response,
                              data = train_filtered.hex,
                              distribution = "multinomial",
                              n.trees = c(10, 20, 40,50),
                              shrinkage = c(0.05, 0.1, 0.2),
                              interaction.depth = c(3,5,7,9),
                              n.minobsinnode =c(5,10,15,20),
                              nfolds=5)

gbm_fit <- h2o.gbm(x = predictors,
               y = response,
               data = train_filtered.hex,
               distribution = "multinomial",
               n.trees = 20,
               shrinkage = 0.1,
               interaction.depth = 5,
               n.minobsinnode =20,
               n.bins=20,
               nfolds=5)

gbm_fit@model$auc

predictions_gbm <- as.data.frame(h2o.predict(gbm_fit, test_filtered.hex ))


ensemble_predict <- 0.4*predictions_gbm[,3] + 0.6*submission_best$prediction

predictions <- cbind(bidder_id = as.character(submission_best$bidder_id),prediction = ensemble_predict)
write.csv(predictions,"rf_gbm_rf_50.csv",row.names=FALSE)

predROCR = prediction(data.frame(ensemble_predict), aggregated_test$outcome)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values

gbm
top_gbm <- gbm@model[[1L]]




log_model_search_0 <- h2o.glm(x = predictors, y = response,
                              data = train_filtered.hex,
                              family = "binomial",
                              lambda_search = TRUE,
                              nlambda = 10,
                              return_all_lambda = TRUE,
                              alpha = 0,
                              nfolds=5)
                              
glm_model <- h2o.glm(x = predictors, y = response,
                                 data = train_filtered.hex,
                                 family = "binomial",
                                 lambda = 3.477200e-03, #0.007491401,  
                                 alpha = 0,
                                 nfolds=5)


predictions_glm <- as.data.frame(h2o.predict(glm_model, test_filtered.hex ))
predictions <- cbind(bidder_id = as.character(aggregated_test$bidder_id),prediction = predictions_glm[,3])
write.csv(predictions,"rf_pred_glm_ridge.csv",row.names=FALSE)





predictions_nn <- data.frame(matrix(0, ncol = 1, nrow = 4700))
for (i in 1:25){
nn_model <- h2o.deeplearning(x=predictors,
                          y=response,
                          data=train_filtered.hex,
                          classification=T,
                          activation="RectifierWithDropout",
                          hidden=c(22,22),
                          hidden_dropout_ratio=c(0.5,0.5),
                          input_dropout_ratio=0.05,
                          epochs=100,
                          l1=3.477200e-03, #3.477200e-03,
                          #l2= 0.007491401,
                          epsilon=1e-8,
                          train_samples_per_iteration=61,
                          max_w2=10,
                          ,nfolds=5)

nn_model
pred <- as.data.frame(h2o.predict(nn_model, test_filtered.hex ))[3]
predictions_nn <- predictions_nn + pred
}
predictions_nn2 <- data.frame(predictions_nn)

predictions_nn_mean <- data.frame(prediction = predictions_nn/25)
predictions_nn_mean <- cbind(bidder_id = aggregated_test$bidder_id,prediction = predictions_nn_mean[,1])
write.csv(predictions_nn_mean,"nn_pred_h2o.csv",row.names=FALSE)
