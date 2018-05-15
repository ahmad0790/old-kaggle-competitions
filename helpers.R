deciles_evaluation <-
  function(test,actual_field, predicted_field, cutoff)
  {
    #predicted_field = "glm_pred"
    #actual_field = "high_refund_ind"
    #cutoff =0.6
    predicted_test_quantiles <- with(test, quantile(test[,predicted_field], probs = c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,1)))
    temp <- within(test, decile<- cut(test[,predicted_field], breaks = predicted_test_quantiles, labels = 1:10, include.lowest = TRUE))
    predicted_classes<-as.numeric(temp[,predicted_field] >= quantile(temp[,predicted_field],cutoff))
    temp<- cbind(temp, predicted_classes)
    
    # evaluation of model with predicted deciles outlook
    predicted_deciles_eval<-cbind(aggregate(temp[,actual_field], by=list(temp$decile), FUN=sum), aggregate(temp[,actual_field], by=list(temp$decile), FUN=length)[2])
    total <- sum(temp[,actual_field])
    predicted_deciles_eval<-cbind(predicted_deciles_eval, (predicted_deciles_eval[,2]/predicted_deciles_eval[,3])*100, predicted_deciles_eval[,2]/total*100)
    names(predicted_deciles_eval)<-c("Predicted_Decile", "Churn_User_Count", "Total_User_Count", "Churn_Rate", "Churned_Percentage_of_All")
    predicted_deciles_eval
    predicted_deciles_eval$Predicted_Decile <- as.factor(predicted_deciles_eval$Predicted_Decile)
    return(predicted_deciles_eval)
  }


accuracy_threshold <- 
  function(actual_data, predicted_data){
    accuracy_data <- data.frame(accuracy_threshold = as.numeric(), sensitivity = as.numeric(), specificity = as.numeric(), difference = as.numeric() )
    for (i in seq(from=0.1, to= 1, by=0.05))
    {
      cutoff_threshold <- quantile(predicted_data,i)
      accuracy_close<-table(actual_data, predicted_data >= cutoff_threshold)
      true_positives <- accuracy_close[4]
      true_negatives <- accuracy_close[1]
      false_positives <- accuracy_close[3]
      false_negatives <- accuracy_close[2]
      specificity <- accuracy_close[1]/(accuracy_close[3] + accuracy_close[1])
      sensitivity <- accuracy_close[4]/(accuracy_close[2] + accuracy_close[4])
      precision <-accuracy_close[4]/(accuracy_close[4]+accuracy_close[3])
      recall <- accuracy_close[4]/(accuracy_close[4]+accuracy_close[2]) 
      accuracy <- (accuracy_close[1] + accuracy_close[4])/sum(accuracy_close[1:4])
      temp <- cbind(accuracy_threshold = i, true_positives, true_negatives, false_positives, false_negatives, sensitivity, specificity, precision, recall, accuracy, difference_sens_spec = sensitivity - specificity)
      accuracy_data <- rbind(accuracy_data, temp)
    }
    return(accuracy_data)
  }

clean_data <- function(data) {
  for(i in 1:ncol(data)){
    if(is.numeric(data[,i])){
      data[,i] <-ifelse(data[,i]> quantile(data[,i],0.98,na.rm=TRUE),quantile(data[,i],0.98,na.rm=TRUE),data[,i])
      data[is.na(data[,i]),i] = 0
    }else{
      data[,i] = as.character(data[,i])
      data[is.na(data[,i]),i] = "Missing"
      data[,i] = as.factor(data[,i])
    }
  }
  return(data)
} 

gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}
