function (N, k, R2, confidence) {
    ## n is sample size
    ## k is the number of variables
    ## Rsq is the squared multiple correlation
    ## conlev is the confidence level
    

    ## Import functions
    source("errorcheck.R")
    ncpCalc <- dget("ncpCalc.R")


    ## Error checking
    areShort(N, k, RS, clevel)
    areIntegers(N, k)
    areBetween0And1(RS, clevel)

    if (k < 2) {
        stop("There must be at least two variables.")
    }

    if (N <= k) {
        stop("There must be more observations than variables.")
    }


    ## Derive inputs
    percentile.lower <- (1 - confidence) / 2
    percentile.upper <- confidence + percentile.lower
    df1 <- k - 1
    df2 <- N - df1 - 1
    adjR2 <- 1 - ((1 - R2) * (N - 1)) / df2
    ncp.initial <- N * (adjR2 / (1 - adjR2)) ## noncentrality parameter
    Fobs <- (R2 / df1) / ((1 - R2) / df2)


    ## Calculate the lower limit
    lower.limit <- ncpCalc(percentile.lower, percentile.upper, df1, df2, ncp.initial, N, Fobs)


    ## Calculate the upper limit
    upper.limit <- ncpCalc(percentile.upper, percentile.lower, df1, df2, ncp.initial, N, Fobs)


    ## Calculate the lower bound
    confidence <- confidence - (1 - confidence)
    percentile.upper <- 1 - ((1 - confidence) / 2)
    percentile.lower <- (1 - confidence) / 2
    lower.bound <- ncpCalc(percentile.lower, percentile.upper, df1, df2, ncp.initial, N, Fobs)

    ## Calculate the p level
    plevel <- 1 - pf(Fobs, df1, df2, ncp=0)


    ## Format output
    output.matrix <- matrix(c(lower.limit, upper.limit, lower.bound, plevel), nrow=1)
    colnames(output.matrix) <- c('Lower Limit', 'Upper Limit', 'Lower Bound', 'Plevel')
    output.matrix <- round(output.matrix, 5)
    output.matrix[output.matrix == 1] <- "> 0.99999"
    output.matrix[output.matrix == 0] <- "< 0.00001"


    return(output.matrix)

}
