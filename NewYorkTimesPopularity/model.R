setwd('/Users/ahkhan/Documents/Kaggle/Analytics Edge')

########### BEGINNING OF SCRIPT ##########################
#Reading Data
nytimesdata <- read.csv('NYTimesBlogTrain.csv')
nytimesValidated <- read.csv('NYTimesBlogTest.csv')
#str(nytimesdata)

nytimesValidated$Popular <- NA
nytimesValidated <- nytimesValidated[,c(1:8,10,9)]
nytimes_merged <- rbind(nytimesdata,nytimesValidated)
nytimes_merged$Popular <- as.factor(nytimes_merged$Popular)

##feature engineering
nytimes_merged$weekday <- factor(wday(as.POSIXlt(nytimes_merged$PubDate, format="%Y-%m-%d %H:%M:%S"), label = TRUE), ordered = FALSE)
nytimes_merged$hour <- hour(as.POSIXlt(nytimes_merged$PubDate, format="%Y-%m-%d %H:%M:%S"))
nytimes_merged$month <- factor(month(as.POSIXlt(nytimes_merged$PubDate, format="%Y-%m-%d %H:%M:%S"), label = TRUE), ordered = FALSE)

nytimes_merged$question_mark <- as.factor(grepl("\\?", nytimes_merged$Headline))
nytimes_merged$Headline <- tolower(nytimes_merged$Headline)
nytimes_merged$H_geographyWord <- as.factor(grepl(paste("\\<",geographyWords,"\\>", collapse = "|",sep=""), nytimes_merged$Headline))

#only popular words
popular <- nytimes_merged[which(nytimes_merged$Popular==1),]
not_popular <- nytimes_merged[which(nytimes_merged$Popular==0),]

##GETTING HEADLINE POPULAR WORDS
CorpusHeadline = Corpus(VectorSource(popular$Headline))
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

dtmHeadline = DocumentTermMatrix(CorpusHeadline)
sparseHeadline = removeSparseTerms(dtmHeadline, 0.995)
HeadlineWords = as.data.frame(as.matrix(sparseHeadline))

HeadlineNames <- colnames(HeadlineWords)

###GETTING ABSTRACT POPULAR WORDS
#CorpusAbstract = Corpus(VectorSource(popular$Abstract))
#CorpusAbstract = tm_map(CorpusAbstract, tolower)
#CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)
#CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
#CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))
#CorpusAbstract = tm_map(CorpusAbstract, stemDocument)

#dtmAbstract = DocumentTermMatrix(CorpusAbstract)
#sparseAbstract = removeSparseTerms(dtmAbstract, 0.99)
#AbstractWords = as.data.frame(as.matrix(sparseAbstract))

#AbstractNames = colnames(AbstractWords)

for (i in 1:length(HeadlineNames))
{
  nytimes_merged[,ncol(nytimes_merged)+1] <- grepl(HeadlineNames[i], nytimes_merged$Headline)
  nytimes_merged[,ncol(nytimes_merged)] <- as.numeric(nytimes_merged[,ncol(nytimes_merged)])
  names(nytimes_merged)[ncol(nytimes_merged)] <- paste0("PH_", HeadlineNames[i])
}

##Now re creating headline corpus on entire data set but removing all words that are not popular and frequent
CorpusHeadline = Corpus(VectorSource(nytimes_merged$Headline))
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

dtmHeadline = DocumentTermMatrix(CorpusHeadline)
sparseHead = removeSparseTerms(dtmHeadline, 0.99)
HeadlineWords = as.data.frame(as.matrix(sparseHead))
colnames(HeadlineWords) <- paste0("H_", colnames(HeadlineWords))

###GETTING ABSTRACT POPULAR WORDS
CorpusAbstract = Corpus(VectorSource(nytimes_merged$Abstract))
CorpusAbstract = tm_map(CorpusAbstract, tolower)
CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)
CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))
CorpusAbstract = tm_map(CorpusAbstract, stemDocument)

dtmAbstract = DocumentTermMatrix(CorpusAbstract)
sparseAbstract = removeSparseTerms(dtmAbstract, 0.99)
AbstractWords = as.data.frame(as.matrix(sparseAbstract))
# Let's make sure our variable names are okay for R:
colnames(AbstractWords) <- paste0("A_", colnames(AbstractWords))


##Merging data
trim <- nytimes_merged[,-c(4,5,6,8,10)]
HeadlinesAbstractData <- cbind(HeadlineWords,AbstractWords,trim,row.names=NULL)
HeadlinesAbstract <- cbind(HeadlineWords,AbstractWords,row.names=NULL)

nytimesModelingData <- head(HeadlinesAbstractData, nrow(nytimesdata))
nytimesFinalValidation <- tail(HeadlinesAbstractData, nrow(nytimesValidated))

#descrCorr <- cor(HeadlinesAbstract)
#ighCorr <- findCorrelation(descrCorr, 0.80)
#trainDescr <- HeadlinesAbstract[, -highCorr]
#testDescr <- testDescr[, -highCorr]
#ncol(trainDescr)

#Splitting our given "test" data into 2 sub parts for evaluation purposes
set.seed(144)
nytimesModelingData$Popular <- as.factor(ifelse(nytimesModelingData$Popular==1,"Yes","No"))
spl = sample.split(nytimesModelingData$Popular, 0.7)
nytimestrain = subset(nytimesModelingData, spl == TRUE)
nytimesTest = subset(nytimesModelingData, spl == FALSE)

#Random Forests
rf_base = randomForest(Popular~.,data = nytimestrain, ntree=100, nodesize = 10)
#rfPredict <- predict(rf_base, newdata = nytimesTest, type = "class")
rf_base_imp<-data.frame(importance(rf_base), type= 1)
rf_base_imp <- rf_base_imp[order(-rf_base_imp[,1]),]
write.csv(rf_base_imp,"rfvarImp_2.csv")
rfPredict <- predict(rf_base, newdata = nytimesTest,type="prob")
rfPredict <- rfPredict[,2]

predROCR = prediction(rfPredict, nytimesTest$Popular)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
# Compute AUC
performance(predROCR, "auc")@y.values

#RF TRIMMED
rf_trim_vars <- rownames(subset(rf_base_imp,rf_base_imp$MeanDecreaseGini < 0.01))
for (i in 1:length(rf_trim_vars)){
  nytimestrain[,rf_trim_vars[i]] <- NULL
}
for (i in 1:length(rf_trim_vars)){
nytimesTest[,rf_trim_vars[i]] <- NULL
}

rf_trim = randomForest(Popular~.,data = nytimestrain, ntree=100, nodesize = 10)

###GBM#######
gbmGrid <-  expand.grid(interaction.depth = c(3,4,5,6),
                       n.trees = (1:5)*50,
                       shrinkage = c(0.05, 0.1))

myControl_2 <- trainControl(method='cv', number=3, returnResamp='none',classProbs=TRUE)

model_gbm <- train(Popular~., data=nytimestrain, 
                   method='gbm', trControl=myControl_2, verbose = TRUE,
                   tuneGrid = gbmGrid)

predictions_gbm <- predict(model_gbm, newdata = nytimesTest, type="prob")
predictions_gbm <- predictions_gbm[,2]

predROCR = prediction(predictions_gbm, nytimesTest$Popular)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values

#0.9474291 #tuned=0.9302704-ntrees=250,interaction.depth=3,shrinkage=0.1

#model_gbm_2 <- gbm(Popular~., data=nytimestrain,n.trees=150,distribution="bernoulli",interaction.depth=3,shrinkage=0.1)
#gbm_preds <- predict(object=model_gbm_2, newdata=nytimesTest,type="response",n.trees=150)

##Logistic Regression
nytimestrain$WordCount = log(nytimestrain$WordCount+1)
nytimesTest$WordCount = log(nytimesTest$WordCount+1)
nytimesTest$NewsDesk <- as.factor(ifelse(nytimesTest$NewsDesk=="Sports","Culture",as.character(nytimesTest$NewsDesk)))
nytimesTest$SectionName <- as.factor(ifelse(nytimesTest$SectionName=="Sports","Multimedia",as.character(nytimesTest$SectionName)))

nytimesLog <- glm(Popular~.,data = nytimestrain,family="binomial")
nytimesLogPredict <- predict(nytimesLog,newdata=nytimesTest,type="response")
table(nytimesTest$Popular, nytimesLogPredict >= 0.5)

predROCR = prediction(nytimesLogPredict, nytimesTest$Popular)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values

###EVALUATION

#confusionmatrix <-table(nytimesTest$Popular,rfPredict>=0.5) 
#accuracy <- (confusionmatrix[1]+confusionmatrix[4])/nrow(nytimesTest)
#confusionmatrix
#accuracy

#confusionmatrix <-table(nytimesTest$Popular,nytimesLogPredict>=0.1) 
#accuracy <- (confusionmatrix[1]+confusionmatrix[4])/nrow(nytimesTest)
#confusionmatrix
#accuracy

#confusionmatrix <-table(nytimesTest$Popular,predictions_gbm>=0.5) 
#accuracy <- (confusionmatrix[1]+confusionmatrix[4])/nrow(nytimesTest)
#confusionmatrix
#accuracy

##RF Validation
rf_validated <- randomForest(Popular~.,data = nytimesModelingData, ntree=500, nodesize = 10)
rfPredict_validated <- predict(rf_validated, newdata = nytimesFinalValidation, type = "prob")
rfPredict_validated <- rfPredict_validated[,2]
#as.numeric(format(rfPredict_validated, digits = 6))

##GBM VALIDATION
gbmValGrid <-  expand.grid(interaction.depth = 3,
                        n.trees = 250,
                        shrinkage = 0.1)
myControl_2 <- trainControl(method='cv', number=3, returnResamp='none',classProbs=TRUE)
gbmValid <- train(Popular~., data=nytimesModelingData, method='gbm', 
                  trControl=myControl_2, tuneGrid =gbmValGrid)
gbmPredValid <- predict(gbmValid, newdata = nytimesFinalValidation, type="prob")
gbmPredValid <- gbmPredValid[,2]

##LogRegresion
nytimesFinalValidation$WordCount = log(nytimesModelingData$WordCount+1)
nytimesFinalValidation$NewsDesk <- as.factor(ifelse(nytimesFinalValidation$NewsDesk=="Sports","Culture",as.character(nytimesTest$NewsDesk)))
nytimesFinalValidation$SectionName <- as.factor(ifelse(nytimesFinalValidation$SectionName=="Sports","Multimedia",as.character(nytimesTest$SectionName)))
nytimesFinalValidation$SectionName <- as.factor(ifelse(nytimesFinalValidation$month=="Dec","Nov",as.character(nytimesTest$month)))


nytimesLogValidated <- glm(Popular~.,data = nytimesModelingData,family="binomial")
nytimesLogPredictValid <- predict(nytimesLogValidated,newdata=nytimesFinalValidation,type="response")

averagePredVal <-  (0.3*gbmPredValid+0.7*rfPredict_validated)

##SUBMISSION
submission_data <- data.frame(UniqueID = nytimesValidated$UniqueID, Probability1 = averagePredVal)
write.csv(submission_data,"submission_v5.csv", row.names=FALSE)

predROCR = prediction(averagePred, nytimesTest$Popular)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values


write.csv(submission_data,"submission_v2.csv", row.names=FALSE)
