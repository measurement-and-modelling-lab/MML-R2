function (data) {

    is.square <- ncol(data) == nrow(data)

    ## If the upper triangle of a correlation matrix is empty, make the matrix symmetric
    ## Otherwise, check whether the matrix is symmetric and if not return an error
    if (is.square) {
        current.upper.triangle <- data[upper.tri(data)]
        symmetric.upper.triangle <- t(data)[upper.tri(t(data))]
        if (all(is.na(current.upper.triangle))) {
            data[upper.tri(data)] <- symmetric.upper.triangle
        } else if (!all(current.upper.triangle == symmetric.upper.triangle)) {
            stop("Input matrix is not symmetric.")
        }
        if (FALSE %in% (diag(data) == 1)) {
            data <- cov2cor(data)
        }
    }

    ## Check for missing data
    if (NA %in% as.numeric(data)) {
        stop("Your data has missing or non-numeric elements.")
    }

    ## Convert raw data to correlation data
    if (!is.square) {
        data <- cor(data)
    }

    ## Check that the data matrix is positive definite
    eigen.values <- eigen(data)[[1]]
    if (TRUE %in% (eigen.values <= 0)) {
        stop('Data matrix is not positive definite.')
    }

    return(data)
}
