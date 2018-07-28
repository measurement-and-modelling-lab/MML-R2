function (N, k, R2, confidence) {
    ## N is sample size
    ## k is the number of variables
    ## R2 is the squared multiple correlation
    ## confidence is the confidence level

    ## Import functions
    limitCalc <- dget("limitCalc.R")
    SensibleRounding <- dget("SensibleRounding.R")

    ## Derive inputs
    percentile.lower <- (1 - confidence) / 2
    percentile.upper <- confidence + percentile.lower
    df1 <- k - 1
    df2 <- N - df1 - 1
    adjR2 <- 1 - ((1 - R2) * (N - 1)) / df2
    ncp <- N * (adjR2 / (1 - adjR2))
    Fobs <- (R2 / df1) / ((1 - R2) / df2)

    ## Calculate the lower limit
    lower.limit <- limitCalc(percentile.lower, percentile.upper, df1, df2, ncp, N, Fobs)

    ## Calculate the upper limit
    upper.limit <- limitCalc(percentile.upper, percentile.lower, df1, df2, ncp, N, Fobs)

    ## Calculate the lower bound
    confidence <- confidence - (1 - confidence)
    percentile.lower <- (1 - confidence) / 2
    percentile.upper <- confidence + percentile.lower
    lower.bound <- limitCalc(percentile.lower, percentile.upper, df1, df2, ncp, N, Fobs)

    ## Calculate the p level
    plevel <- 1 - pf(Fobs, df1, df2, ncp=0)

    ## Assemble output matrix
    output.matrix <- matrix(c(lower.limit, upper.limit, lower.bound, plevel), nrow=1)
    colnames(output.matrix) <- c('Lower Limit', 'Upper Limit', 'Lower Bound', 'plevel')
    output.matrix <- SensibleRounding(output.matrix, 5)

    return(output.matrix)

}
