function (n, k, Rsq, conlev) {
    ## n is sample size
    ## k is the number of variables
    ## Rsq is the squared multiple correlation
    ## conlev is the confidence level

    maxiter <- 50 ## maximum number of iterations
    pul <- (1-conlev)/2
    pll <- 1-(1-conlev)/2
    df1 <- n-1
    df2 <- n-k-1
    Rsqtilde <- Rsq/(1-Rsq)

    check <- pf(df2*Rsqtilde/k,k,df2)

    if (!is.finite(check)) {
        stop("Confidence interval calculation failed.")
    }

    if (check <= pul) {
        lower = NULL
        upper = NULL
    }

    if (check <= pll) {
        lower <- NULL
    }

    for (type in 0:1) {
        if (type == 1) {
            criterion <- pul ## To Find Upper Limit
        } else {
            criterion <- pll ## To Find Lower Limit
        }

        x1 <- 0
        x2 <- 1
        x3 <- 0
        diff3 <- 1

        if (check > criterion) {

            while (abs(diff3) > 1e-6) {
                for (i in 1:maxiter) {
                    for (root in 1:2) {
                        x3 <- (x1 + x2)/2
                        if (root == 1) {
                            yy <- x3/(1-x3)
                        } else {
                            yy <- x1/(1-x1)
                        }

                        gamma <- sqrt(1+yy)
                        phi1 <- df1*(gamma**2-1)+k
                        phi2 <- df1*(gamma**4-1)+k
                        phi3 <- df1*(gamma**6-1)+k
                        g <- (phi2-(sqrt(phi2**2-phi1*phi3)))/phi1
                        nu <- (phi2-2*yy*gamma*(sqrt(df1*df2)))/g**2
                        lambdau <- yy*gamma*(sqrt(df1*df2))/g**2
                        limit<- df2*Rsqtilde/(nu*g)

                        diff <- pf(limit,nu,df2,lambdau)-criterion
                        if (!is.finite(diff)) {
                            stop("Confidence interval calculation failed.")
                        }

                        if (root == 1){
                            diff3 <- diff
                        } else {
                            diff1 <- diff
                        }
                    }
                }

                if (diff1*diff3 < 0) {
                    x2 <- x3
                } else {
                    x1 <- x3
                    upper <- x3
                }
            }
        }
        if (type==1) {
            upper <- x3
        } else {
            lower <- x3
        }
        if (is.null(lower)) {
            lower <- 0
        }
    }
    return(c(lower, upper))
}
