#detach(package: h2o)
require(bit64)
library(h2o)
library(Metrics)
localH2O = h2o.init(nthread=-1, max_mem_size = "8g")

train.hex <- as.h2o(localH2O,enrollment_train_agg)
test.hex <- as.h2o(localH2O,enrollment_test_agg)

predictors <- c(3,5:ncol(train.hex))
predictors <- c(3,5:36,45,57:ncol(train.hex))
response = 4


gbm_fit <- h2o.gbm(x = predictors,
                   y = response,
                   data = train.hex,
                   distribution = "multinomial",
                   n.trees = 100,
                   shrinkage = 0.1,
                   interaction.depth = 7,
                   n.minobsinnode =20,
                   importance = TRUE,
                   n.bins=20,
                   nfolds=5)

gbm_fit@model$auc

pred_gbm <- h2o.predict(gbm_fit,test.hex)
p <- as.data.frame(pred_gbm)
summary(p)
subm[,2] = p[,3]
summary(subm)
write.table(subm, "submission_gbm_meh.csv",sep=",", row.names=FALSE,col.names=FALSE)



model_all <- h2o.randomForest(x=predictors,y=response,data = train.hex,
                              #sample.rate = 0.5,
                              classification = T
                              ,ntree = 50
                              ,importance=TRUE
                              ,nfolds=5
                              ,seed = 5650987299948753920
)

model_all@model$auc

rf.VI = data.frame(t(model_all@model$varimp))
write.csv(rf.VI, "rf_imp2.csv")
pred <- h2o.predict(model_all,test.hex)
p <- as.data.frame(pred)
p <- p[,3]
subm <- data.frame(cbind(enrollment_test$enrollment_id,pred_ensemble))
write.table(subm, "submission_gbm_lasso_ensemble.csv",sep=",", row.names=FALSE,col.names=FALSE)

log_model_search <- h2o.glm(x = predictors, y = response,
                            data = train.hex,
                            family = "binomial",
                            lambda_search = TRUE,
                            nlambda = 10,
                            return_all_lambda = TRUE,
                            alpha = 1,
                            nfolds=5)
log_model_search

glm_model_lasso <- h2o.glm(x = predictors, y = response,
                           data = train.hex,
                           family = "binomial",
                           lambda = 2.350382e-05/5 ,  
                           alpha = 1,
                           nfolds=5)
glm_model_lasso@model$auc

glm_model_lasso <- h2o.glm(x = predictors, y = response,
                           data = train.hex,
                           family = "binomial",
                           lambda = 0.02196680,  
                           alpha = 0,
                           nfolds=5)
glm_model_lasso@model$auc

pred_lasso <- h2o.predict(glm_model_lasso,test.hex)
pred_lasso <- as.data.frame(pred_lasso)
pred_lasso <- pred_lasso[,3]
summary(p)
subm[,2] = p[,3]
summary(subm)

##with only user we get 0.89096
1,5,
enrollment_train_agg_course <- enrollment_train_agg[which(enrollment_train_agg$course_id == '3cnZpv6ReApmCaZyaQwi2izDZxVRdC01'),]

train.hex <- as.h2o(localH2O,enrollment_train_agg_course)


nn_model <- h2o.deeplearning(x=predictors,
                             y=response,
                             data=train.hex,
                             classification=T,
                             activation="RectifierWithDropout",
                             hidden=c(120,120),
                             hidden_dropout_ratio=c(0.5,0.5),
                             input_dropout_ratio=0.1,
                             epochs=75,
                             l1=2.196680e-05  , #3.477200e-03,
                             #l2= 0.2357711/50,
                             epsilon=1e-8,
                             train_samples_per_iteration=2000,
                             #rate=0.1,
                             #rate_decay = 2**(1.0/4),
                             #momentum_start=0.1,
                             #momentum_stable=0.9,
                             nesterov_accelerated_gradient=T,
                             max_w2=10,
                             ,nfolds=5)
nn_model

pred_nn <- h2o.predict(nn_model,blender.hex)
pred_nn <- as.data.frame(pred_nn)
summary(pred_nn)
subm[,2] = (0.4*pred_nn[,3] + 0.6*xg_pred)
summary(subm)
write.table(subm, "submission_xg_nn.csv",sep=",", row.names=FALSE,col.names=FALSE)
