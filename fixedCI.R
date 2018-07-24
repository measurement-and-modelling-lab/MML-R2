function (N, k, R2, confidence) {

    percentile.lower <- (1 - confidence) / 2
    percentile.upper <- confidence + percentile.lower
    df1 <- k - 1
    df2 <- N - df1 - 1
    adjR2 <- 1 - ((1 - R2) * (N - 1)) / df2
    ncp.initial <- N * (adjR2 / (1 - adjR2)) ## noncentrality parameter
    Fobs <- (R2 / df1) / ((1 - R2) / df2)
    tolerance <- 0.000000001

    ncpCalc <- function (percentile1, percentile2, df1, df2, ncp.initial, N) {

        F.quartile <- qf(percentile1, df1, df2, ncp.initial)              ## calculate the F quartile for the lower percentile, for the initial ncp
        R2.limit <- (F.quartile / df2) / ((1 / df1) + (F.quartile / df2)) ## calculate the limit based on the the F quartile
        ncp.new <- N * (R2.limit / (1 - R2.limit))                        ## recalculate ncp using R2.limit
        density <- pf(Fobs, df1, df2, ncp.new)                            ## calculate the percentage of the F distribution to the left of ncp.new
        density.difference <- abs(density - percentile2)                  ## calculate the difference between that percentage and the upper percentile

        while (density.difference > tolerance) { ## repeat the calculation until the difference is very small
            density.new <- pf(Fobs, df1, df2, ncp.new)
            ncp.new <- -((density.new - percentile2) / (density.new - 0.5)) * (ncp.new - ncp.initial) + ncp.new
            density.difference <- abs(density.new - percentile2)

            if (ncp.new < 0) {
                ncp.new <- 0
                break
            }
            
        }

        R2.limit <- ncp.new / (ncp.new + N)

        return(R2.limit)

    }

    ## Calculate lower limit
    lower.limit <- ncpCalc(percentile.lower, percentile.upper, df1, df2, ncp.initial, N)

    ## Calculate upper limit
    upper.limit <- ncpCalc(percentile.upper, percentile.lower, df1, df2, ncp.initial, N)

    ## Calculate lower bound
    confidence <- confidence - (1 - confidence)
    percentile.upper <- 1 - ((1 - confidence) / 2)
    percentile.lower <- (1 - confidence) / 2
    lower.bound <- ncpCalc(percentile.lower, percentile.upper, df1, df2, ncp.initial, N)

    ## Calculate p level
    plevel <- 1 - pf(Fobs, df1, df2, ncp=0)

    ## Format output
    output.matrix <- matrix(c(lower.limit, upper.limit, lower.bound, plevel), nrow=1)
    colnames(output.matrix) <- c('Lower Limit', 'Upper Limit', 'Lower Bound', 'Plevel')
    output.matrix <- round(output.matrix, 5)
    output.matrix[output.matrix == 1] <- "> 0.99999"
    output.matrix[output.matrix == 0] <- "< 0.00001"

    return(output.matrix)

}
