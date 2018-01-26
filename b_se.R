data <- read.csv("/home/paul/git/MML-R2/stderror/rawdata.csv", header = FALSE, sep = ",")
print(data)

n <- nrow(data)
p <- ncol(data)-1 # number of predictors

Y <- data[,1] # the raw data for the criterion
X <- data[,-1] # the raw data for the predictors

sxy <- cov(data)[1,-1] # covariance of the criterion with each predictor
sxx <- cov(X) # covariance of the predictors
sxxInv <- solve(sxx)
sy <- sd(Y)

betaHat <- sxxInv %*% sxy # unstandardised coefficients

alphaHat <- mean(Y) - t(betaHat) %*% colMeans(X) # inercept
alphaHat <- rep(alphaHat, nrow(data))

X <- as.matrix(X)

SEtbj <- matrix(0, nrow=p, ncol=1)

sigmaHat <- (1/(n-p-1)) * sum((Y - alphaHat - t(betaHat) %*% t(X))^2)
sigmaHat <- sqrt(sigmaHat)

for (j in 1:p) {

  sj <- sqrt(sxx[j,j])
  aj <- sqrt(sxxInv[j,j])
  
  SEtbj[j,1] <- (sj * aj * sigmaHat)/(sqrt(n) * sy) # standard error of the jth standardised predictor

}

print(SEtbj)