function (data, X, Y) {
    ## data is a raw, correlation, or covariance matrix
    ## X is a vector of positive integers corresponding to two or more columns of X to use as predictors
    ## Y is a single positive integer corresponding to a column of X to use as the criterion

    ## Import functions
    RoundPercentile <- dget("RoundPercentile.R")

    ## Calculate R2
    Rxy <- data[X, Y, drop=FALSE] # single column matrix of the the correlations between Y and each predictor
    Rxx <- data[X, X] # matrix of the correlations between the predictors
    b <- solve(Rxx) %*% Rxy
    R2 <- t(b) %*% Rxy

    ## Round R2
    R2 <- RoundPercentile(R2)

    return(R2)
}
