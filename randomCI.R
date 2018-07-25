function (n, k, Rsq, conlev) {
    ## n is sample size
    ## k is the number of variables
    ## Rsq is the squared multiple correlation
    ## conlev is the confidence level

    ## Import functions
    source("errorcheck.R")
    Bisection <- dget("Bisection.R")
    SensibleRounding <- dget("SensibleRounding.R")

    ## Error checking
    areShort(n, k, Rsq, conlev)
    areIntegers(n, k)
    areBetween0And1(Rsq, conlev)
    if (k < 2) {
        stop("There must be at least two variables!")
    }
    if (n <= k) {
        stop("There must be more observations than variables.")
    }

    ## Run Bisections
    bisection.output <- Bisection(n, k, Rsq, conlev)
    upper <- bisection.output[2]
    lower <- bisection.output[1]
    lbound <- Bisection(n, k, Rsq,  conlev-(1-conlev))[1]

    ## Asssemble input matrix
    output.table <- matrix(c(lower, upper, lbound), nrow=1, ncol=3)
    colnames(output.table) <- c('Lower Limit', 'Upper Limit', 'Lower Bound')
    
    ## Round output
    output.table <- SensibleRounding(output.table, 5)

    return(output.table)

}
