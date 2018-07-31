function (n, k, Rsq, conlev) {
    ## n is sample size
    ## k is the number of variables
    ## Rsq is the squared multiple correlation
    ## conlev is the confidence level

    ## Import functions
    Bisection <- dget("Bisection.R")
    RoundPercentile <- dget("RoundPercentile.R")

    ## Run Bisections
    bisection.output <- Bisection(n, k, Rsq, conlev)
    lower <- bisection.output[1]
    upper <- bisection.output[2]
    lbound <- Bisection(n, k, Rsq,  conlev-(1-conlev))[1]

    ## Asssemble input matrix
    output.table <- matrix(c(lower, upper, lbound), nrow=1, ncol=3)
    colnames(output.table) <- c('Lower Limit', 'Upper Limit', 'Lower Bound')
    
    ## Round output
    output.table <- RoundPercentile(output.table)

    return(output.table)

}
