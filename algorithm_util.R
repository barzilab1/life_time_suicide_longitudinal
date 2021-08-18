library(glmnet)
library(ROCR)
library(stir)
library(randomForest)


#lasso 1, ridge  = 0
run_elastic_net <- function(x_train, y_train, x_test, y_test, alpha) {
  
  #find best lambda
  mod <- cv.glmnet(x=as.matrix(x_train),y=y_train,alpha=alpha,family="binomial")
  opt_lambda <- mod$lambda.min
  
  #get model
  mod <- mod$glmnet.fit
  
  #get selected features 
  coefs <- coef(mod, s=opt_lambda)
  
  y_predicted <- predict(mod, s = opt_lambda, newx = as.matrix(x_test),type ="response")
  pred <- prediction(y_predicted, y_test)
  
  measurments = calculate_measurments(pred)
  
  return(list(
    #get all selected features not including the intercept 
    features = coefs@Dimnames[[1]][ which(coefs != 0 ) ][-1],
    measurments = measurments
  ))
  
  
}

print_lasso <- function(lasso_features, lasso_measurements ) {
  
  #calculate and print results
  cat("\n Lasso results \n")
  
  #get features
  cat("\nFeatures\n")
  lasso_features = lasso_features[order(lasso_features[,1], decreasing = TRUE),,drop = F]
  print(lasso_features)
  plot(lasso_features ,xlab="" ,ylab="Frequency", xaxt="n" , main="lasso", pch = 19)
  axis(1, at=1:nrow(lasso_features), labels=rownames(lasso_features))
  
  # avg. number of selected features. 
  cat("\nAvg. selected features")
  cat("\nall 10k models: " , sum(lasso_features)/N_SPLITS, sep = "\t")  
  cat("\n")
  
  #get measurements closest to (0,1)
  cat("\nmeasurements: \n")
  print(apply(lasso_measurements, 1, mean, na.rm = T))
  cat("\nSD: \n")
  print(apply(lasso_measurements, 1, sd, na.rm = T))
  cat("\n")
  
}



##########################################
# relieff (according to P_value)
##########################################

run_stir <- function(x_train, y_train) {
  
    RF.method = "multisurf"
    metric = "manhattan"
    neighbors <- find.neighbors(x_train, y_train, k = 0, method = RF.method)
    res <- stir(x_train, neighbors, k = 0, metric = metric, method = RF.method)$STIR_T
    res <- rownames(res[ (!is.na(res$t.pval) & res$t.pval < 0.05),])
    return(res)
      
}
  
print_stir <- function(stir_features){

  cat("\n\nSelected Features According to Relieff\n")
  stir_features = stir_features[order(stir_features, decreasing = TRUE),,drop=FALSE]
  print(stir_features)
  plot(stir_features ,xlab="" ,ylab="Frequency", xaxt="n" , main="stir", pch = 19)
  axis(1, at=1:nrow(stir_features), labels=rownames(stir_features))
  
  cat("\n Avg. selected features")
  cat("\nall 10k models: " , sum(stir_features)/N_SPLITS, sep = "\t")  
  cat("\n")
  
}



##########################################
# Random Forest
##########################################

run_rf <- function(x_train, y_train, x_test, y_test) {
  
  mode_rf = tuneRF(x=x_train,y=as.factor(y_train),doBest = T, importance = TRUE)

  # mode_rf <- randomForest(x=x_train,y=as.factor(y_train), importance = TRUE)
 

  #calculate measurements
  y_predicted <- predict(mode_rf, newdata = x_test, type ="prob")[,2]
  pred <- prediction(y_predicted, y_test)
  
  measurments = calculate_measurments(pred)
  
  
  return(list(
    #get all selected features not including the intercept 
    features = as.matrix(mode_rf$importance[,"MeanDecreaseGini"]),
    measurments = measurments
  ))
  
}

print_rf <- function(rf_features, rf_measurements){
  
  cat("\n\nSelected Features According to Random Forest\n")
  
  #MeanDecreaseGini
  features_MeanDecreaseGini = rf_features[order(rf_features[,1], decreasing = TRUE),1,drop=FALSE]
  print(features_MeanDecreaseGini)
  plot(features_MeanDecreaseGini ,xlab="" ,ylab="", xaxt="n" , main="MeanDecreaseGini", pch = 19)
  axis(1, at=1:nrow(rf_features), labels=rownames(features_MeanDecreaseGini))

  #get measurements closest to (0,1)
  cat("\nmeasurements: \n")
  print(apply(rf_measurements, 1, mean, na.rm = T))
  cat("\nSD: \n")
  print(apply(rf_measurements, 1, sd, na.rm = T))
  cat("\n")  
  
}


# x: a dataframe with all the features not including bblid
run_rf_ridge = function(x,y,features_names){
  
  cl = makeCluster(N_CORES, type="FORK")
  registerDoParallel(cl)
  
  results_list = list()
  results_list = foreach(i = seq(N_SPLITS), .packages=c("missForest","glmnet","ROCR","caTools") 
                         ) %dopar% {
                           
                           set.seed(i)
                           ridge_auc = list()
                           rf_auc = list()

                          #' (1) imputation 
                          if(imputation){
                            
                            #split the data splits times to 75% training and 25% test
                            splitz = sample.split(y, .75)
                            x_train <- x[splitz,]
                            y_train <- y[splitz]
                            x_test <- x[!splitz,]
                            y_test <- y[!splitz]
                            
                            x_train = missForest(x_train)$ximp
                            x_test = missForest(x_test)$ximp
                            
                          }else{
                            # remove rows with NA and then split
                            # in order to have always 2 class in y
                            index_not_missing = which(rowSums(is.na(x)) == 0)
                            x_clean = x[index_not_missing,]
                            y_clean = y[index_not_missing]

                            splitz = sample.split(y_clean, .75)
                            x_train <- x_clean[splitz,]
                            y_train <- y_clean[splitz]
                            x_test <- x_clean[!splitz,]
                            y_test <- y_clean[!splitz]

                          }
                          
                           #keep the seed so each algo will be independent in reproducing results
                           seed = .Random.seed
                           
                          for(feature_set in names(features_names)){
                            
                            #get data according to bucket
                            train_data = x_train[,features_names[[feature_set]]]
                            test_data = x_test[,features_names[[feature_set]]]
                            
                            ###ridge
                            .Random.seed = seed
                            res_ridge = run_elastic_net(train_data, y_train, test_data, y_test,0)
                            ridge_auc[[feature_set]] =  res_ridge$measurments["auc"]
                            
                            ###rf
                            .Random.seed = seed
                            res_rf = run_rf(train_data, y_train, test_data, y_test)
                            rf_auc[[feature_set]] =  res_rf$measurments["auc"]
                            
                          }
                           
                           return(list(
                             ridge_auc = ridge_auc,
                             rf_auc = rf_auc
                           ))
                          
                         }
  
  stopCluster(cl)
  
  
  ridge_auc = list()
  rf_auc = list()
  
  for(feature_set in names(features_names)){
    ridge_auc[[feature_set]] = sapply(results_list, function(x){x$ridge_auc[[feature_set]]})
    rf_auc[[feature_set]] = sapply(results_list, function(x){x$rf_auc[[feature_set]]})
        
  }
  
  
  for(feature_set in names(rf_auc)){
    
    cat("\n##################################\n")
    cat(feature_set)
    cat("\n##################################\n")
    
    cat("ridge auc: ",mean(ridge_auc[[feature_set]]))
    cat("\nridge sd: ", sd(ridge_auc[[feature_set]]))
    cat("\nrf auc: ",mean(rf_auc[[feature_set]]))
    cat("\nrf sd: ", sd(rf_auc[[feature_set]]),"\n")
    
  }
  
  return(list("ridge_auc" = ridge_auc,"rf_auc" = rf_auc))
  
}


run_lasso_stir_rf = function(x,y) {
  
  cl = makeCluster(N_CORES, type="FORK")
  registerDoParallel(cl)
  
  results_list = list()
  
  results_list = foreach(i = seq(N_SPLITS), .packages=c("missForest","glmnet","ROCR","stir","caTools") 
                         ) %dopar% {
                           set.seed(i)
                           
                           #' (1) imputation 
                           if(imputation){
                             
                             #split the data splits times to 75% training and 25% test
                             splitz = sample.split(y, .75)
                             x_train <- x[splitz,]
                             y_train <- y[splitz]
                             x_test <- x[!splitz,]
                             y_test <- y[!splitz]
                             
                             x_train = missForest(x_train)$ximp
                             x_test = missForest(x_test)$ximp
                             
                           }else{
                             # remove rows with NA and then split
                             # in order to have always 2 class in y
                             index_not_missing = which(rowSums(is.na(x)) == 0)
                             x_clean = x[index_not_missing,]
                             y_clean = y[index_not_missing]
                             
                             splitz = sample.split(y_clean, .75)
                             x_train <- x_clean[splitz,]
                             y_train <- y_clean[splitz]
                             x_test <- x_clean[!splitz,]
                             y_test <- y_clean[!splitz]
                             
                           }
                           
                           #keep the seed so each algo will be independent in reproducing results
                           seed = .Random.seed
                           
                           .Random.seed = seed
                           res_lasso = run_elastic_net(x_train, y_train, x_test, y_test,1)
                           
                           .Random.seed = seed
                           res_stir = run_stir(x_train, y_train)
                           
                           .Random.seed = seed
                           res_rf = run_rf(x_train, y_train, x_test, y_test)
                           
                           return(list(
                             res_lasso = res_lasso,
                             res_stir = res_stir,
                             res_rf = res_rf
                           ))
                           
                         }
  
  stopCluster(cl)
  
  #lasso
  lasso_features = as.matrix(table(unlist(sapply(results_list, function(x) {x$res_lasso$features}))))
  lasso_measurements = sapply(results_list, function(x) {x$res_lasso$measurments})
  print_lasso(lasso_features, lasso_measurements)
  
  missing_lasso = setdiff(colnames(x),rownames(lasso_features))
  lasso_features = rbind(lasso_features, matrix(0,nrow = length(missing_lasso), ncol = 1))
  rownames(lasso_features)[(length(x) - length(missing_lasso) +1):length(x)] = missing_lasso
  
  #stir
  stir_features = as.matrix(table(unlist(sapply(results_list, function(x) {x$res_stir}))))
  print_stir(stir_features)
  
  missing_stir = setdiff(colnames(x),rownames(stir_features))
  stir_features = rbind(stir_features, matrix(0,nrow = length(missing_stir), ncol = 1))
  rownames(stir_features)[(length(x) - length(missing_stir) +1):length(x)] = missing_stir
  
  #rf
  rf_features_list = lapply(results_list, function(x) {x$res_rf$features})
  rf_features_df = Reduce(function(fetures1, features2){
    cbind(fetures1, features2[match(rownames(fetures1), rownames(features2))])}, 
    rf_features_list)
  
  rf_features = as.matrix(rowMeans(rf_features_df))
  rf_measurements = sapply(results_list, function(x) {x$res_rf$measurments})
  print_rf(rf_features, rf_measurements)
  
  
  return(list(
    lasso = list(features = lasso_features,
                 number_selected = ceiling(sum(lasso_features)/N_SPLITS)
    ),
    stir = list(features = stir_features,
                number_selected = ceiling(sum(stir_features)/N_SPLITS)
    ),
    rf = list(features = rf_features)
  )
  )
  
}





