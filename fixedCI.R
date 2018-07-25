function (N, k, R2, confidence) {
    ## n is sample size
    ## k is the number of variables
    ## Rsq is the squared multiple correlation
    ## conlev is the confidence level
    

    ## Import functions
    source("errorcheck.R")
    limitCalc <- dget("limitCalc.R")
    SensibleRounding <- dget("SensibleRounding.R")

    ## Error checking
    areShort(N, k, R2, confidence)
    areIntegers(N, k)
    areBetween0And1(R2, confidence)
    if (k < 2)  stop("There must be at least two variables.")
    if (N <= k) stop("The sample size must be greater than the number of variables.")


    ## Derive inputs
    percentile.lower <- (1 - confidence) / 2
    percentile.upper <- confidence + percentile.lower
    df1 <- k - 1
    df2 <- N - df1 - 1
    adjR2 <- 1 - ((1 - R2) * (N - 1)) / df2
    ncp.initial <- N * (adjR2 / (1 - adjR2)) ## noncentrality parameter
    Fobs <- (R2 / df1) / ((1 - R2) / df2)


    ## Calculate the lower limit
    lower.limit <- limitCalc(percentile.lower, percentile.upper, df1, df2, ncp.initial, N, Fobs)


    ## Calculate the upper limit
    upper.limit <- limitCalc(percentile.upper, percentile.lower, df1, df2, ncp.initial, N, Fobs)


    ## Calculate the lower bound
    confidence <- confidence - (1 - confidence)
    percentile.upper <- 1 - ((1 - confidence) / 2)
    percentile.lower <- (1 - confidence) / 2
    lower.bound <- limitCalc(percentile.lower, percentile.upper, df1, df2, ncp.initial, N, Fobs)


    ## Calculate the p level
    plevel <- 1 - pf(Fobs, df1, df2, ncp=0)


    ## Format output
    output.matrix <- matrix(c(lower.limit, upper.limit, lower.bound, plevel), nrow=1)
    colnames(output.matrix) <- c('Lower Limit', 'Upper Limit', 'Lower Bound', 'Plevel')
    output.matrix <- SensibleRounding(output.matrix, 5)


    return(output.matrix)

}
