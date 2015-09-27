require(xgboost)
require(methods)
library(ggplot2)
library(caret)

enrollment_train_agg <- enrollment_train_agg[,predictors]
enrollment_test_agg <- enrollment_test_agg[,predictors]

##KCIKING OFF SPLITT
y = as.character(enrollment_train_agg$dropout)
y = as.integer(y)

#enrollment_train_agg <- enrollment_train_agg[,-43]
x = rbind(enrollment_train_agg[,5:ncol(enrollment_train_agg)],enrollment_test_agg[,4:ncol(enrollment_test_agg)])
x = as.matrix(x)
x = matrix(as.numeric(x),nrow(x),ncol(x))

trind = 1:length(y)
teind = (nrow(enrollment_train_agg)+1):nrow(x)

# Set necessary parameter

param <- list("objective" = "binary:logistic",
              "max_depth"= 6,
              "eta"= 0.1,
              "eval_metric" = "auc",
              "nthread" = 8)


# Run Cross Valication
cv.nround = 250
bst.cv = xgb.cv(param=param, data = x[trind,], label = y, 
                nfold = 5, nrounds=cv.nround,missing="NAN")

# Train the model
nround = 98
bst = xgboost(param=param, data = x[trind,], label = y, nrounds=nround,missing="NAN")

# Make prediction
xg_pred = as.numeric(predict(bst,x[teind,],missing="NAN"))

subm <- data.frame(cbind(enrollment_test$enrollment_id,xg_pred))
write.table(subm, "submission_xgboost_3.csv",sep=",", row.names=FALSE,col.names=FALSE)
