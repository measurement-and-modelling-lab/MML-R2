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
    R2 <- round(R2, 5)

    R2 <- matrix(R2, nrow=1, ncol=1)
    colnames(R2) <- "R<sup>2</sup>"

    return(R2)
    
}
