function (k, rho, alpha, power) {

    source("errorcheck.R")
    
    ## Check for errors
    areShort(k, rho, alpha, power)
    areIntegers(k)
    areBetween0And1(rho, alpha, power)
    if (k < 2) {
        stop("There must be at least two variables.")
    }

    ## Define values
    df1 <- k - 1
    N <- 1000 ## starting point
    i <- 1
    increment <- 1000 ## Special value used for bisection range finding
    count <- 0
    tol <- 0.00000001 ## Tolerance
    found <- FALSE

    while(!found){

        df2 <- N - df1 - 1
        Fobs <- (rho/df1)/((1-rho)/(N-df1-1))
        nc <- N*(rho/(1-rho))

        ## Error check
        check <- c()
        check[1] <- suppressWarnings(qf(1-alpha,df1,df2))
        check[2] <- suppressWarnings(pf(check[1], df1, df2, ncp=nc))
        if (FALSE %in% is.finite(check)) {
            stop("Sample size calculation failed.")
        }

        fcrit <- qf(1-alpha, df1, df2)

        powerest <- 1-pf(fcrit, df1, df2, ncp=nc) ## Power Estimate

        ## Modified bisection
        if (powerest > power) {
            increment <- increment/2
            N <- N-increment
        } else if (powerest < power) {
            N <- N + increment ##Increment the N estimate by increment until it goes above the original estimate
        }

        if (abs(powerest - power) < tol) {
            found <- TRUE
        }

        count <- count + 1
        if (count > 1000) {
            stop("Sample size calculation failed")
        }
    }

    Nlow <- floor(N)
    Nhigh <- ceiling(N)


    ## Calculate power using the lower N and the higher N
    Power <- dget("Power.R")
    powerlow <- Power(Nlow,k,rho,alpha)
    powerhigh <- Power(Nhigh,k,rho,alpha)


    ## Assemble output matrix
    output.table <- matrix(c(powerlow, powerhigh), nrow=1, ncol=2)
    rowname1 <- paste0("N = ", Nlow)
    rowname2 <- paste0("N = ", Nhigh)
    colnames(output.table) <- c(rowname1, rowname2)
    rownames(output.table) <- "<b>Power</b>"

    
    ## Round output
    output.table <- round(output.table, 5)
    output.table[output.table == 1] <- "> 0.99999"
    output.table[output.table == 0] <- "< 0.00001"

    return(output.table)
}
