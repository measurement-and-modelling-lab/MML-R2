data <- read.csv("rawdata.csv", header = FALSE, sep = ",")

n <- nrow(data)
p <- ncol(data)-1

Y <- data[,1]
X <- data[,-1]

sxy <- cov(data)[1,-1]
sxx <- cov(X)
sxxInv <- solve(sxx)
sy <- sd(Y)

j <- 1
sj <- sqrt(sxx[j,j])
aj <- sqrt(sxxInv[j,j])

betaHat <- sxxInv %*% sxy

alphaHat <- mean(Y) - t(betaHat) %*% colMeans(X)
alphaHat <- rep(alphaHat, nrow(data))

X <- as.matrix(X)
X <- t(X) # The paper doesn't say to do this

sigmaHat <- (1/(n-p-1)) * sum((Y - alphaHat - t(betaHat) %*% X)^2)
sigmaHat <- sqrt(sigmaHat)

SEtbj <- (sj * aj * sigmaHat)/(sqrt(n) * sy)

print(SEtbj)