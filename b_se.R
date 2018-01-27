function (data, predictors, criterion){
  
  n <- nrow(data)
  Y <- data[,criterion] # the raw data for the criterion
  X <- data[,predictors] # the raw data for the predictors
  Y <- as.matrix(Y)
  X <- as.matrix(X)
  p <- ncol(X)
  
  
  
  sxy <- cov(data)[predictors,criterion,drop=FALSE] # covariance of the criterion with each predictor
  sxx <- cov(X) # covariance of the predictors
  
  sxxInv <- solve(sxx)
 
  sy <- sd(Y)


  
  betaHat <- sxxInv %*% sxy # unstandardised coefficients

  alphaHat <- mean(Y) - t(betaHat) %*% colMeans(X) # inercept
  alphaHat <- rep(alphaHat, nrow(data))
  


  
  SEtbj <- matrix(0, nrow=p, ncol=1)
  

  
  sigmaHat <- (1/(n-p-1)) * sum((Y - alphaHat - X %*% betaHat)^2)
  
  sigmaHat <- sqrt(sigmaHat)
  
  
  
  for (j in 1:p) {
  
    sj <- sqrt(sxx[j,j])
    aj <- sqrt(sxxInv[j,j])
    
    SEtbj[j,1] <- (sj * aj * sigmaHat)/(sqrt(n) * sy) # standard error of the jth standardised predictor
  
  
  }
  return (SEtbj)
}

