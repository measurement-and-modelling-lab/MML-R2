function(N, k, rho, alpha){

    df1 <- k-1
    df2 = N - df1 - 1
    Fobs <- (rho/df1)/((1-rho)/(N-df1-1))
    nc = N*(rho/(1-rho))
    
    fcrit <- qf(1-alpha,df1,df2)
    power <- 1-pf(fcrit,df1,df1,ncp=nc)

    return(power)
}
