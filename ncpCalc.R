function (percentile1, percentile2, df1, df2, ncp.initial, N, Fobs) {

    F.quartile <- qf(percentile1, df1, df2, ncp.initial)              ## calculate the F quartile for the lower percentile, for the initial ncp
    R2.limit <- (F.quartile / df2) / ((1 / df1) + (F.quartile / df2)) ## calculate the limit based on the the F quartile
    ncp.new <- N * (R2.limit / (1 - R2.limit))                        ## recalculate ncp using R2.limit
    density <- pf(Fobs, df1, df2, ncp.new)                            ## calculate the percentage of the F distribution to the left of ncp.new
    density.difference <- abs(density - percentile2)                  ## calculate the difference between that percentage and the upper percentile

    tolerance <- 0.000000001
    iteration <- 0
    while (density.difference > tolerance) { ## recalculate until density.difference is very small
        density.new <- pf(Fobs, df1, df2, ncp.new)
        ncp.new <- -((density.new - percentile2) / (density.new - 0.5)) * (ncp.new - ncp.initial) + ncp.new
        density.difference <- abs(density.new - percentile2)

        if (ncp.new < 0) {
            ncp.new <- 0
            break
        }

        iteration <- iteration + 1
        if (iteration > 1000) {
            stop("Confidence interval calculation failed.")
        }

    }

    R2.limit <- ncp.new / (ncp.new + N)

    return(R2.limit)

}
