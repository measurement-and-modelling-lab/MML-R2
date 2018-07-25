function(N, k, rho, alpha, round){
    ## N is sample size
    ## k is the number of variables
    ## alpha is the type I error rate
    ## round is a logical indicating whether to round the output

    ## Derive key values from parameters
    df1 <- k - 1
    df2 = N - df1 - 1
    Fobs <- (rho / df1)/((1 - rho)/(N - df1 - 1))
    ncp = N * (rho / (1 - rho)) ## noncentrality parameter

    ## Calculate fcrit and power; if either fails, return a clean error message
    fcrit <- qf(1-alpha, df1, df2)
    tryCatch(power <- pf(fcrit, df1, df2, ncp=ncp),
             warning = function(w) stop("Power calculation failed."), 
             error = function(e) stop("Power calculation failed."))

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
