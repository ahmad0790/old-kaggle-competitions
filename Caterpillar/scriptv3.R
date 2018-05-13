set.seed(2)
setwd("/Users/ahkhan/Documents/Kaggle/Caterpillar")
base = "/Users/ahkhan/Documents/Kaggle/Caterpillar/competition_data/"
test = read.csv(paste0(base, "test_set.csv"))
train = read.csv(paste0(base, "train_set.csv"))

train$id = -(1:nrow(train))
test$cost = 0

dFull = rbind(train, test)

dFull = merge(dFull, read.csv(paste0(base, "bill_of_materials.csv"), quote = ""), by = "tube_assembly_id", all.x = TRUE)
dFull = merge(dFull, read.csv(paste0(base, "specs.csv"), quote = ""), by = "tube_assembly_id", all.x = TRUE)
dFull = merge(dFull, read.csv(paste0(base, "tube.csv"), quote = ""), by = "tube_assembly_id", all.x = TRUE)
compFiles = dir(base)[grep("comp_", dir(base))]

  
idComp = 1
keyMerge = 0
for(idComp in 1:8){
  for(f in compFiles){
    d = read.csv(paste0(base, f), sep = ',', quote = "")
    names(d) = paste0(names(d), "_", keyMerge)
    dFull = merge(dFull, d, by.x = paste0("component_id_", idComp), by.y = paste0("component_id_", keyMerge), all.x = TRUE)
    keyMerge = keyMerge + 1
  }
  cat("idComp = ", idComp, " - nrow(dFull) = ", nrow(dFull), " and ncol(dFull) = ", ncol(dFull), "\n")
}


### Feature engineering
## Mat volume
dFull$vol_full<-dFull$diameter^2*3.1415957/4*dFull$length
dFull$vol_wall<-(dFull$diameter^2-(dFull$diameter-dFull$wall)^2)*3.1415957/4*dFull$length

## Date
date<-as.POSIXlt(dFull$quote_date)
dFull$year<-date$year
dFull$month<-date$mon
dFull$cummonth<-date$year*12+date$mon

## Tube assemblies
dFull$tube_assembly_id<-as.numeric(substr(dFull$tube_assembly_id,4,8))

### Clean NA values
for(i in 1:ncol(dFull)){
  if(is.numeric(dFull[,i])){
    dFull[is.na(dFull[,i]),i] = 0
  }else{
    dFull[,i] = as.character(dFull[,i])
    dFull[is.na(dFull[,i]),i] = "NAvalue"
    dFull[,i] = as.factor(dFull[,i])
  }
}

dFull$component_1_ind <-ifelse(dFull$component_id_1 == 'NAvalue',0,1)
dFull$component_2_ind <-ifelse(dFull$component_id_2 == 'NAvalue',0,1)
dFull$component_3_ind <-ifelse(dFull$component_id_3 == 'NAvalue',0,1)
dFull$component_4_ind <-ifelse(dFull$component_id_4 == 'NAvalue',0,1)
dFull$component_5_ind <-ifelse(dFull$component_id_5 == 'NAvalue',0,1)
dFull$component_6_ind <-ifelse(dFull$component_id_6 == 'NAvalue',0,1)
dFull$component_7_ind <-ifelse(dFull$component_id_7 == 'NAvalue',0,1)
dFull$component_8_ind <-ifelse(dFull$component_id_8 == 'NAvalue',0,1)

dFull$spec_1_ind <-ifelse(dFull$spec1 == 'NAvalue',0,1)
dFull$spec_2_ind <-ifelse(dFull$spec2 == 'NAvalue',0,1)
dFull$spec_3_ind <-ifelse(dFull$spec3 == 'NAvalue',0,1)
dFull$spec_4_ind <-ifelse(dFull$spec4 == 'NAvalue',0,1)
dFull$spec_5_ind <-ifelse(dFull$spec5 == 'NAvalue',0,1)
dFull$spec_6_ind <-ifelse(dFull$spec6 == 'NAvalue',0,1)
dFull$spec_7_ind <-ifelse(dFull$spec7 == 'NAvalue',0,1)
dFull$spec_8_ind <-ifelse(dFull$spec8 == 'NAvalue',0,1)
dFull$spec_9_ind <-ifelse(dFull$spec9 == 'NAvalue',0,1)
dFull$spec_10_ind <-ifelse(dFull$spec10 == 'NAvalue',0,1)

dFull$total_components <- dFull$component_1_ind+dFull$component_2_ind+dFull$component_3_ind+dFull$component_4_ind+dFull$component_5_ind+dFull$component_6_ind+dFull$component_7_ind+dFull$component_8_ind

dFull$component_quantity <- dFull$quantity_1+dFull$quantity_2+dFull$quantity_3+dFull$quantity_4+dFull$quantity_5+dFull$quantity_6+dFull$quantity_7+dFull$quantity_8

dFull$total_specs <- dFull$spec_1_ind+dFull$spec_2_ind+dFull$spec_3_ind+dFull$spec_4_ind+dFull$spec_5_ind+dFull$spec_6_ind+dFull$spec_7_ind+dFull$spec_8_ind+dFull$spec_9_ind+dFull$spec_10_ind

#dFull$cost_per_volume <- dFull$cost/dFull$vol_full

dFull$number_ends <-ifelse(dFull$end_a == 'NONE',0,1) + ifelse(dFull$end_x == 'NONE',0,1)+ifelse(dFull$end_a_1x == 'N',0,1)+ifelse(dFull$end_a_2x == 'N',0,1)+ifelse(dFull$end_x_1x == 'N',0,1)+ifelse(dFull$end_x_2x == 'N',0,1)

dFull$usage_per_quanity <- dFull$annual_usage/dFull$quantity

dFull$dimensions <- dFull$diameter+dFull$wall+dFull$length + dFull$num_bends + dFull$num_boss + dFull$num_bracket +dFull$other

dFull$tot_bend_radius <- dFull$bend_radius*dFull$num_bends

dFull$total_weight <- dFull$weight_8+dFull$weight_6+dFull$weight_5+dFull$weight_32+dFull$weight_30+dFull$weight_28+dFull$weight_18+dFull$weight_17+dFull$weight_13

dFull$density <- (0.01+dFull$total_weight*9.8)/(dFull$vol_full+0.01)

dFull$quant_dimensions <- dFull$quantity/dFull$dimensions 

dFull$vol_full_wall <- dFull$vol_full/dFull$vol_wall 


featuresadded <- names(dFull)[1178:ncol(dFull)]

### Clean variables with too many categories
for(i in c(1:ncol(dFull))){
  if(!is.numeric(dFull[,i])){
    freq = data.frame(table(dFull[,i]))
    freq = freq[order(freq$Freq, decreasing = TRUE),]
    dFull[,i] = as.character(match(dFull[,i], freq$Var1[1:55]))
    dFull[is.na(dFull[,i]),i] = "rareValue"
    dFull[,i] = as.factor(dFull[,i])
  }
}

print(dim(dFull))
featuresTop100=c("quantity","component_id_1","annual_usage","vol_full","supplier","vol_wall","diameter",
                 "quote_date","min_order_quantity","end_a","component_id_2","component_id_3","end_x",
                 "cummonth","quantity_1","length","bend_radius","bracket_pricing","tube_assembly_id",
                 "spec2","material_id","spec1","year","spec3","quantity_3","wall","month","num_bends",
                 "part_name_6","quantity_2","part_name_17","spec5","spec4","thread_size_5","thickness_8",
                 "component_type_id_8","drop_length_24","weight_6","part_name_28","weight_8",
                 "bolt_pattern_long_8","connection_type_id_1_10","component_type_id_32","end_form_id_1_32",
                 "weight_17","end_x_2x","end_a_1x","end_a_2x","extension_length_24","end_form_id_2_32",
                 "orientation_32","length_5","length_18","end_x_1x","unique_feature_24","spec6","weight_32",
                 "groove_8","head_diameter_8","end_form_id_1_10","length_2_32","component_type_id_5",
                 "extension_length_2","orientation_10","weight_28","intended_nut_thread_18","thread_size_16",
                 "nominal_size_2_32","groove_24","base_type_1","component_id_4","hex_size_10",
                 "base_diameter_1","thread_pitch_5","weight_13","thread_size_1_10","height_over_tube_1",
                 "unique_feature_18","weight_5","nominal_size_2_10","overall_length_32","other",
                 "orientation_8","drop_length_2","component_type_id_10","thickness_24",
                 "connection_type_id_1_32","overall_length_10","unique_feature_32","weight_30",
                 "num_boss","component_type_id_24","intended_nut_pitch_18","connection_type_id_2_32",
                 "mj_class_code_8","thread_pitch_1_32","weight_18","groove_2","end_form_id_2_10","unique_feature_13")

dFull<-dFull[,c("id","cost",featuresTop100, featuresadded)]
print(dim(dFull))

dFull$quote_date <- NULL

test = dFull[which(dFull$id > 0),]
train = dFull[which(dFull$id < 0),]

train$id <- -1*train$id

train_log <- train
train_log$cost <- log(train_log$cost+1)

#train_log$quantity_decile <- ntile(train_log$quantity, 5)
#train_log$diameter_decile <- ntile(train_log$diameter, 5)  

#test$quantity_decile <- ntile(test$quantity, 5)
#test$diameter_decile <- ntile(test$diameter, 5)  

#train_log$cost_decile <- ntile(train_log$cost, 10)
#test$cost_decile <- ntile(test$cost, 10) 

comp_supplier_cost <-
  train_log %>%
  group_by(component_id_1,supplier, quantity_decile) %>%
  summarize(component_avg_cost = mean(cost)
  )

comp_2_supplier_cost <-
  train_log %>%
  group_by(component_id_1, component_id_2,supplier, quantity_decile) %>%
  summarize(component_avg_cost_2 = mean(cost)
  )

comp_1_diam_decile <-
  train_log %>%
  group_by(component_id_1,supplier,diameter_decile) %>%
  summarize(component_quantity_avg_cost_1 = mean(cost)
  )

supplier_quant_cost <-
  train_log %>%
  group_by(supplier,bracket_pricing,quantity_decile) %>%
  summarize(supplier_quantity_avg_cost_1 = mean(cost)
  )  

supplier_cost <-
  train_log %>%
  group_by(supplier,bracket_pricing) %>%
  summarize(supplier_avg_cost = mean(cost)
  )  


component_cost_1 <-
  train_log %>%
  group_by(component_id_1) %>%
  summarize(component_cost_1 = mean(cost)
  )  

component_cost_2 <-
  train_log %>%
  group_by(component_id_2) %>%
  summarize(component_cost_2 = mean(cost)
  ) 

component_cost_3 <-
  train_log %>%
  group_by(component_id_3) %>%
  summarize(component_cost_3 = mean(cost)
  )  

train_log <- left_join(train_log, comp_supplier_cost, by = c('component_id_1','supplier','quantity_decile'))
train_log <- left_join(train_log, comp_2_supplier_cost, by = c('component_id_1', 'component_id_2','supplier', 'quantity_decile'))
train_log <- left_join(train_log, comp_1_diam_decile, by = c('component_id_1','supplier','diameter_decile'))
train_log <- left_join(train_log, supplier_quant_cost, by = c('supplier','bracket_pricing','quantity_decile'))
train_log <- left_join(train_log, supplier_cost, by = c('supplier','bracket_pricing'))
train_log <- left_join(train_log, component_cost_1, by = c('component_id_1'))
train_log <- left_join(train_log, component_cost_2, by = c('component_id_2'))
train_log <- left_join(train_log, component_cost_3, by = c('component_id_3'))

test <- left_join(test, comp_supplier_cost, by = c('component_id_1','supplier','quantity_decile'))
test <- left_join(test, comp_2_supplier_cost, by = c('component_id_1', 'component_id_2','supplier', 'quantity_decile'))
test <- left_join(test, comp_1_diam_decile, by = c('component_id_1','supplier','diameter_decile'))
test <- left_join(test, supplier_quant_cost, by = c('supplier','bracket_pricing', 'quantity_decile'))
test <- left_join(test, supplier_cost, by = c('supplier','bracket_pricing'))
test <- left_join(test, component_cost_1, by = c('component_id_1'))
test <- left_join(test, component_cost_2, by = c('component_id_2'))
test <- left_join(test, component_cost_3, by = c('component_id_3'))

#vars_to_drop <- c('component_id_1', 'component_id_2', 'component_id_3', 'supplier')
#train_log <- train_log[, !(colnames(train_log) %in% vars_to_drop)]
#test <- test[, !(colnames(test) %in% vars_to_drop)]


train_log <- as.data.frame(train_log)
vars_to_keep <- c('cost', 'quantity','end_a,supplier','component_id_3','component_id_1'
                  ,'bracket_pricing','component_id_2','vol_full','annual_usage'
                  ,'dimensions','vol_wall','diameter','cummonth','month','usage_per_quanity'
                  ,'spec2','year','id','bend_radius','drop_length_24'
                  ,'part_name_28','part_name_6','component_id_4','end_x','end_a_1x')

train_log <- train_log[, (colnames(train_log) %in% vars_to_keep)]
tr.mf  <- model.frame(as.formula(paste("cost ~",paste(names(train_log),collapse = "+"))),train_log)
tr.m  <- model.matrix(attr(tr.mf,"terms"),data = train)

train_log_matrix <- as.data.frame(tr.m)



#rm(dFull)

# - match removes the specified column from set
#test = test[,-match("id", names(test))] train = train[,-match("id",
#names(train))]

cat("Final train dataset : ", nrow(train_log), " rows and ", ncol(train_log), "
    columns\n")
cat("Final test dataset : ", nrow(test), " rows and ", ncol(test), " columns\n")


localH2O = h2o.init(nthread=-1, max_mem_size = "8g")

train_log <- subset(train_log, quantity_decile != 1)

offset = 6000
start = 1

h2o_train<-as.h2o(localH2O,train_log)
h2o_test<-as.h2o(localH2O,test)

gbm = h2o.gbm(x = c(1,3:ncol(h2o_train)), y = 2, 
              training_frame = h2o_train,
              distribution="AUTO",
              nfolds = 5,
              seed = 666,
              ntrees = 100, #1500
              max_depth = 7, 
              min_rows = 5,
              learn_rate = 0.1) #0.05

pred_quantity <- h2o.predict(gbm, h2o_train)
h2o_train$quantity_error <- h2o_train$quantity - pred_quantity$predict

gbm_new = h2o.gbm(x = c(1,3:ncol(h2o_train)), y = 2, 
              training_frame = h2o_train,
              distribution="AUTO",
              nfolds = 5,
              seed = 666,
              ntrees = 1000, #1000
              max_depth = 7, 
              min_rows = 5,
              learn_rate = 0.05) #0.05

pred<-as.data.frame(h2o.predict(gbm_new, h2o_test))
pred_gbm <- exp(pred$predict) - 1
pred_gbm<-pred$predict
write.csv(data.frame(id=test$id, cost=pred_gbm),"gbm_submit_log_v3",row.names=F, quote=FALSE)

h2o_train<-as.h2o(localH2O,train_log)
h2o_test<-as.h2o(localH2O,test)

gbm = h2o.gbm(x = c(1,3:ncol(h2o_train)), y = 2, 
                  training_frame = h2o_train,
                  distribution="AUTO",
                  nfolds = 5,
                  seed = 666,
                  ntrees = 1500, #1000
                  max_depth = 7, 
                  min_rows = 5,
                  learn_rate = 0.02) #0.05
pred1<-as.data.frame(h2o.predict(gbm, h2o_test))

gbm2 = h2o.gbm(x = c(1,3:ncol(h2o_train)), y = 2, 
                  training_frame = h2o_train,
                  distribution="AUTO",
                  nfolds = 5,
                  seed = 666,
                  ntrees = 3000, #1000
                  max_depth = 7, 
                  min_rows = 5,
                  learn_rate = 0.02) #0.05
pred2<-as.data.frame(h2o.predict(gbm2, h2o_test))

gbm4 = h2o.gbm(x = c(1,3:ncol(h2o_train)), y = 2, 
                  training_frame = h2o_train,
                  distribution="AUTO",
                  nfolds = 5,
                  seed = 666,
                  ntrees = 4000, #1000
                  max_depth = 7, 
                  min_rows = 5,
                  learn_rate = 0.02) #0.05

pred4 <-as.data.frame(h2o.predict(gbm4, h2o_test))

h2o_train$cost <- (train_cost)*(1/16)
gbm3 = h2o.gbm(x = c(1,3:ncol(h2o_train)), y = 2, 
                  training_frame = h2o_train,
                  distribution="AUTO",
                  nfolds = 5,
                  seed = 666,
                  ntrees = 4000, #1000
                  max_depth = 7, 
                  min_rows = 5,
                  learn_rate = 0.02) #0.05

pred3<-as.data.frame(h2o.predict(gbm4, h2o_test))

preds = 0.4*exp(pred4)+.1*exp(pred1)+0.1*exp(pred2)+0.4*(pred3^16)





