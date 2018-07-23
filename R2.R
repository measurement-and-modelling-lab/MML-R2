function (data, X, Y) {

    ## If the upper triangle of a correlation matrix is empty, make the matrix symmetric
    ## Otherwise, check whether the matrix is symmetric and if not return an error
    is.square <- ncol(data) == nrow(data)
    if (is.square) {
        current.upper.triangle <- data[upper.tri(data)]
        symmetric.upper.triangle <- t(data)[upper.tri(t(data))]
        if (all(is.na(current.upper.triangle))) {
            data[upper.tri(data)] <- symmetric.upper.triangle
        } else if (!all(current.upper.triangle == symmetric.upper.triangle)) {
            stop("Correlation matrix is not symmetric.")
        }
    } else {
        data <- cor(data)
    }

    ## If the matrix is a covariance matrix, convert it
    if (FALSE %in% (diag(data) == 1)) {
        data <- cov2cor(data)
    }

    ## Check that the correlation matrix is positive definite
    eigen.values <- eigen(data)[[1]]
    if (TRUE %in% (eigen.values <= 0)) {
        stop('Data matrix is not positive definite.')
    }

    if (Y %in% X) {
        stop("A variable cannot be both a predictor and the criterion.")
    }

    if (NA %in% data) {
        stop("Your data has missing or non-numeric elements.")
    }

    Rxy <- data[X, Y, drop=FALSE] # single column matrix of the the correlations between Y and each predictor
    Rxx <- data[X, X] # matrix of the correlations between the predictors
    b <- solve(Rxx) %*% Rxy
    R2 <- t(b) %*% Rxy

    ## Round output
    R2 <- round(R2, 5)
    if (R2 == 1) {
        R2 <- "> 0.99999"
    } else if (R2 == 0) {
        R2 <- "< 0.00001"
    } else {
        R2 <- paste0("= ", R2)
    }

    return(R2)
}
