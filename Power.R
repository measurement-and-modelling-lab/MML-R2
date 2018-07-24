function(N, k, rho, alpha, round){
    ## N is sample size
    ## k is the number of variables
    ## alpha is the type I error rate
    ## round is a logical indicating whether to round the output

    ## Error checking
    source("errorcheck.R")
    areShort(N, k, rho, alpha)
    areIntegers(N, k)
    areBetween0And1(rho, alpha)
    if (k < 2) {
        stop("There must be at least two variables!")
    }
    if (N <= k) {
        stop("There must be more observations than variables.")
    }

    df1 <- k-1
    df2 = N - df1 - 1
    Fobs <- (rho/df1)/((1-rho)/(N-df1-1))
    nc = N*(rho/(1-rho))

    ## More error checking
    check <- c()
    check[1] <- suppressWarnings(qf(1-alpha,df1,df2))
    check[2] <- suppressWarnings(pf(check[1], df1, df2, ncp=nc))
    if (FALSE %in% is.finite(check)) {
        stop("Power calculation failed.")
    }
    
    fcrit <- qf(1-alpha,df1,df2)
    power <- 1-pf(fcrit,df1,df1,ncp=nc)


    ## Round output if requested
    if (round) {
        power <- round(power, 5)
        if (power == 1) {
            power <- "> 0.99999"
        } else if (power == 0) {
            power <- "< 0.00001"
        } else {
            power <- paste0("= ", power)
        }
    }

    return(power)
}
