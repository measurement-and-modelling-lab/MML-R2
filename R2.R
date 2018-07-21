function (data, X, Y) {

    if (nrow(data) != ncol(data)) {
        data <- cor(data)
    }

    if (FALSE %in% (diag(data) == 1)) {
        data <- cov2cor(data)
    }

    Rxy <- data[X, Y, drop=FALSE] # single column matrix of the the correlations between Y and each predictor
    Rxx <- data[X, X] # matrix of the correlations between the predictors
    b <- solve(Rxx) %*% Rxy
    R2 <- t(b) %*% Rxy

    return(R2)
}
