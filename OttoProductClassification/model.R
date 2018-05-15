require(xgboost)
require(methods)
library(ggplot2)
library(caret)

setwd("/Users/ahkhan/Documents/Kaggle/Otto")


train = read.csv('train.csv',header=TRUE,stringsAsFactors = F)
test = read.csv('test.csv',header=TRUE,stringsAsFactors = F)
test$target <- NA
merged <- rbind(train,test)

#x <- data.frame(model.matrix(~(feat_1+feat_2+feat_3+feat_4+feat_5+feat_6+feat_7+feat_8+feat_9+feat_10+feat_11+feat_12+feat_13+feat_14+feat_15+feat_16+feat_17+feat_18+feat_19+feat_20+feat_21+feat_22+feat_23+feat_24+feat_25+feat_26+feat_27+feat_28+feat_29+feat_30+feat_31+feat_32+feat_33+feat_34+feat_35+feat_36+feat_37+feat_38+feat_39+feat_40+feat_41+feat_42+feat_43+feat_44+feat_45+feat_46+feat_47+feat_48+feat_49+feat_50+feat_51+feat_52+feat_53+feat_54+feat_55+feat_56+feat_57+feat_58+feat_59+feat_60+feat_61+feat_62+feat_63+feat_64+feat_65+feat_66+feat_67+feat_68+feat_69+feat_70+feat_71+feat_72+feat_73+feat_74+feat_75+feat_76+feat_77+feat_78+feat_79+feat_80+feat_81+feat_82+feat_83+feat_84+feat_85+feat_86+feat_87+feat_88+feat_89+feat_90+feat_91+feat_92+feat_93)^2,merged))
#merged <- x


merged$feat_11_high <- factor(ifelse(merged$feat_11>=6&merged$feat_11<=10,1,0))
merged$feat_9_high <- as.factor(ifelse(merged$feat_9>=9&merged$feat_9<=18,1,0))
merged$feat_13_high <- as.factor(ifelse(merged$feat_13>=10&merged$feat_13<=11,1,0))
merged$feat_15_high <- as.factor(ifelse(merged$feat_15>=15&merged$feat_15<=23,1,0))
merged$feat_32_high <- as.factor(ifelse(merged$feat_32>=6&merged$feat_32<=7,1,0))
merged$feat_60_high <- as.factor(ifelse(merged$feat_60>=8&merged$feat_60<=12,1,0))

merged$feat_60_color <- as.factor(ifelse(merged$feat_60>=5,1,0))
merged$feat_77_color <- as.factor(ifelse(merged$feat_77>=18,1,0))
merged$feat_3_color <- as.factor(ifelse(merged$feat_3>=3,1,0))
merged$feat_4_color <- as.factor(ifelse(merged$feat_4>=3,1,0))
merged$feat_15_color <- as.factor(ifelse(merged$feat_15>=4,1,0))
merged$feat_34_color <- as.factor(ifelse(merged$feat_34>=7,1,0))

k=12
KMC_merged = kmeans(merged[,c(2:94)], centers = k, iter.max = 1000)
clusterGroups_merged = KMC_merged$cluster
merged$clusterGroups = as.factor(clusterGroups_merged)

#formula_matrix <- paste(names(merged[2:94]),collapse="+")

train <- head(merged,nrow(train))
test <- tail(merged,nrow(test))


##KCIKING OFF SPLITT
train = train[,-1]
test = test[,-1]
test$target <- NULL

y = train[,94]
y = gsub('Class_','',y)
y = as.integer(y)-1 #xgboost take features in [0,numOfClass)

x = rbind(train[,-94],test)
x = as.matrix(x)
x = matrix(as.numeric(x),nrow(x),ncol(x))
trind = 1:length(y)
teind = (nrow(train)+1):nrow(x)

# Set necessary parameter

param <- list("objective" = "multi:softprob",
              "max_depth"=9,
              "eta"= 0.05,
              "subsample"=0.7,
              "colsample_bytree"= 1,
              "gamma"=1,
              "min_child_weight"=4,
              "eval_metric" = "mlogloss",
              "num_class" = 9,
              "nthread" = 8)

# Run Cross Valication
cv.nround = 850
bst.cv = xgb.cv(param=param, data = x[trind,], label = y, 
                nfold = 5, nrounds=cv.nround)

# Train the model
nround = 900
bst = xgboost(param=param, data = x[trind,], label = y, nrounds=nround)
##bst2 is the trained model

# Make prediction
pred = as.numeric(predict(bst,x[teind,]))
pred = matrix(pred,9,length(pred)/9)
pred = t(pred)

# Output submission
#pred = format(pred, digits=2,scientific=F) # shrink the size of submission
pred = data.frame(1:nrow(pred),pred)
names(pred) = c('id', paste0('Class_',1:9))
write.csv(pred,file='submission4.csv', quote=FALSE,row.names=FALSE)

average <- cbind(id =pred$id, 0.4*(submission[,2:10]/15)+0.6*(pred[,2:10]))

write.csv(average,file='submission_average8.csv', quote=FALSE,row.names=FALSE)

##IMP FEATURES###

##gives 0.44032 current best for xgboost

#feat_11+feat_60+feat_34+feat_90+feat_15+feat_14+feat_86+feat_40+feat_36+feat_67
