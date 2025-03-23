# Building own lineer regression

train_model <- function(X,y,y_test,x_test,epochs=1000,lr=0.01){
  bias <- 0 
  weights <- rep(0,times=ncol(X))
  y_predicted <- numeric(nrow(X))
  for (a in 1:epochs){
    gradient <- numeric(ncol(X))
    bias_gradient <- 0
    # Predicting
    for (i in 1:nrow(X)){
      total <- 0 
      for (j in 1:ncol(X)){
        total <- total + (X[i,j]*weights[j])}
    y_predicted[i] <- total + bias}
    # Computing Gradient 
    for(j in 1:ncol(X)){
      error <- 0
      diff <- 0 
      for(i in 1:nrow(X)){
        error <- error +(y_predicted[i]-y[i])*X[i,j]
        diff <- diff + y_predicted[i]-y[i]
      }
      gradient[j] <- error/nrow(X)
      bias_gradient <- diff/nrow(X) } 
    # Updating Weights and Bias
    for(i in 1:ncol(X)){
      weights[i]<-  weights[i]-(lr*gradient[i])}
    bias <- bias - (lr*bias_gradient)
  }
  result(bias,weights,x_test,y_test)
  
}
prepare_data <- function(model_data){
  sampleIndex <- sample(1:nrow(model_data),size=0.8*nrow(model_data))
  trainSet <- model_data[sampleIndex,]
  testSet <- model_data[-sampleIndex,]
  y_indx<-as.numeric(readline(prompt="Select the column number for the dependent variable: "))
  x_quantity<-as.numeric(readline(prompt="How many independent variables do you want to select? "))
  y <- trainSet[,y_indx]
  y_test <- testSet[,y_indx]
  x_indx <- numeric(x_quantity)
  for(i in 1:x_quantity){
    x_indx[i]<-as.numeric(readline(prompt="Select the column number for one independent variable: "))
  }
  x <- as.matrix(trainSet[,x_indx])
  x_test <- as.matrix(testSet[,x_indx])
  #Scaling
  x_mean <- apply(x, 2, mean)
  x_sd <- apply(x, 2, sd)
  x <- scale(x, center = x_mean, scale = x_sd)
  x_test <- scale(x_test, center = x_mean, scale = x_sd)
  train_model(x,y,y_test,x_test)
}
result <- function(bias,weights,x_test,y_test){
  y_pred <- numeric(nrow(x_test))
  for (i in 1:nrow(x_test)){
    total <- 0 
    for (j in 1:ncol(x_test)){
      total <- total + (x_test[i,j]*weights[j])}
    y_pred[i] <- total + bias}
  # R^2 
  ss_res <- sum((y_test - y_pred)^2)
  ss_tot <- sum((y_test - mean(y_test))^2)
  r2 <- 1 - (ss_res / ss_tot)
  
  # RMSE 
  rmse <- sqrt(mean((y_test - y_pred)^2))
  
  # MAE 
  mae <- mean(abs(y_test - y_pred))
  
  # Result
  cat("R² (Determination Coefficient):", round(r2, 4), "\n")
  cat("RMSE (Root Mean Squared Error):", round(rmse, 2), "\n")
  cat("MAE (Mean Absolute Error):", round(mae, 2), "\n")
  
  
  return(list(y_pred = y_pred,y_test = y_test))
}
library(MASS)
modelData <- Boston
View(modelData)
modelData <- na.omit(modelData)
prepare_data(modelData)



