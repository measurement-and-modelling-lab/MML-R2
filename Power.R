function(N, k, rho, alpha){

    N <- as.numeric(N)
    df1 <- as.numeric(k)-1
    rho <- as.numeric(rho)
    alpha <- as.numeric(alpha)

    df2 = N - df1 - 1
    Fobs <- (rho/df1)/((1-rho)/(N-df1-1))
    nc = N*(rho/(1-rho))
    
    if (is.na(qf(1-alpha,df1,df2))) {
        stop("Power calculation failed.")
    }
    
    fcrit <- qf(1-alpha,df1,df2)
    power <- round(1-pf(fcrit,df1,df1,ncp=nc), 5)
    if (power == 1) {
        power <- '> 0.99999'
    }

    ##Create formatted output
    output.table <- matrix(power, nrow=1, ncol=1)
    colnames(output.table) <- "Power"
    return(output.table)

    ##cat('<center><b>Power Calculation Results</b>')
    ##cat('<i>N=',N,', k=',k, ', &rho;<sup>2</sup>=',rho,', &alpha;=',alpha,'</i></center>', sep="")

}
