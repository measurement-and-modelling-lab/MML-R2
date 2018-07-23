function(N, k, rho, alpha){

    ## Error checking
    source("errorcheck.R")
    areShort(input$n, input$k, input$rho, input$alpha)
    areIntegers(input$n, input$k)
    areBetween0And1(input$rho, input$alpha)
    if (input$k < 2) {
        stop("There must be at least two variables!")
    }
    if (input$n <= input$k) {
        stop("There must be more observations than variables.")
    }

    df1 <- k-1
    df2 = N - df1 - 1
    Fobs <- (rho/df1)/((1-rho)/(N-df1-1))
    nc = N*(rho/(1-rho))

    ## Error check
    check <- c()
    check[1] <- suppressWarnings(qf(1-alpha,df1,df2))
    check[2] <- suppressWarnings(pf(check[1], df1, df2, ncp=nc))
    if (FALSE %in% is.finite(check)) {
        stop("Power calculation failed.")
    }
    
    fcrit <- qf(1-alpha,df1,df2)
    power <- 1-pf(fcrit,df1,df1,ncp=nc)

    return(power)
}
