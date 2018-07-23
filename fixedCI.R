function (N, k, RS, clevel) {

    llimit <- (1-clevel)/2
    ulimit <- clevel + llimit

    df1 <- k-1
    df2 <- N - df1 - 1
    adjRS <- 1- ((1-RS)*(N-1))/df2
    Fobs <- (RS/df1)/((1-RS)/(df2))
    adjRS = 1- ((1-RS)*(N-1))/df2
    nc <- N*(adjRS/(1-adjRS))

    tol <- 0.00000001


    ## Lower limit
    FLL <- qf(llimit,df1,df2,ncp = nc,lower.tail = TRUE,log.p = FALSE)
    RSLL <- (FLL/df2)/((1/df1)+(FLL/df2))
    ncLL <- N*(RSLL/(1-RSLL))
    iterations <- 0
    while(abs((pf(Fobs,df1,df2,ncp=ncLL))-ulimit) > tol ) {
        Pfncll <- pf(Fobs,df1,df2,ncp=ncLL)
        ncLL <- -((Pfncll-ulimit)/(Pfncll -.5))*(ncLL-nc)+ncLL
        if (ncLL < 0) { ## Should it return an error in this case?
            ncLL <- 0
            break
        }
        iterations <- iterations+1
        if (iterations > 1000) {
            stop("Confidence interval calculation failed.")
        }
    }


    ## Upper limit
    FUL <- qf(ulimit,df1,df2,ncp = nc,lower.tail = TRUE,log.p = FALSE)
    RSUL <- (FUL/df2)/((1/df1)+(FUL/df2))
    ncUL <- N*(RSUL/(1-RSUL))
    iterations <- 0
    while(abs((pf(Fobs,df1,df2,ncp=ncUL))-llimit) > tol ) {
        Pfncul <- pf(Fobs,df1,df2,ncp=ncUL)
        ncUL <- -((Pfncul-llimit)/(Pfncul -.5))*(ncUL-nc)+ncUL
        if (ncUL < 0 || iterations > 1000) {
            stop("Confidence interval calculation failed.")
        }
    }


    ## Lower bound
    clevel2 <- clevel - (1-clevel)
    ulimit <- 1-((1-clevel2)/2)
    llimit <- (1-clevel2)/2
    FLB <- qf(llimit,df1,df2,ncp = nc,lower.tail = TRUE,log.p = FALSE)
    RSLB <- (FLB/df2)/((1/df1)+(FLB/df2))
    ncLB <- N*(RSLB/(1-RSLB))
    iterations <- 0
    while(abs((pf(Fobs,df1,df2,ncp=ncLB))-ulimit)> tol ) {
        Pfnclb <- pf(Fobs,df1,df2,ncp=ncLB)
        ncLB <- -((Pfnclb-ulimit)/(Pfnclb -.5))*(ncLB-nc)+ncLB
        if (ncLB < 0) {
            ncLB <- 0
            break
        }
        iterations <- iterations+1
        if (iterations > 1000) {
            stop("Confidence interval calculation failed.")
        }
    }

    plevel <- 1 - pf(Fobs, df1, N-df1-1, ncp=0)

    output.table <- matrix(c(RSLL, RSUL, RSLB, plevel), nrow=1, ncol=4)
    colnames(output.table) <- c('Lower Limit', 'Upper Limit', 'Lower Bound', 'Plevel')

    return(output.table)
}
