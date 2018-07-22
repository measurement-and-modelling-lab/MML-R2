function(N, k, rho, alpha){

    df1 <- k-1
    df2 = N - df1 - 1
    Fobs <- (rho/df1)/((1-rho)/(N-df1-1))
    nc = N*(rho/(1-rho))

    ## Error check
    check <- c()
    check[1] <- suppressWarnings(qf(1-alpha,df1,df2))
    check[2] <- suppressWarnings(pf(check[1],df1,df1,ncp=nc))
    check[3] <- suppressWarnings(qf(1-alpha,df1,df2))
    if (TRUE %in% is.nan(check)) {
        stop("Power calculation failed.")
    }
    
    fcrit <- qf(1-alpha,df1,df2)
    power <- 1-pf(fcrit,df1,df1,ncp=nc)

    ##Create formatted output
    output.table <- matrix(power, nrow=1, ncol=1)
    colnames(output.table) <- "Power"

    return(output.table)
}
