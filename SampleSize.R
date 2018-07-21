function (k, rho, alpha, power) {


    df1 <- k - 1
    N <- 1000
    i <- 1
    increment <- 1000 ## Special value used for bijection range finding
    count <- 0
    tol <- 0.00000001 ## Tolerance
    found <- FALSE

    while(!found){

        df2 <- N - df1 - 1
        Fobs <- (rho/df1)/((1-rho)/(N-df1-1))
        nc <- N*(rho/(1-rho))

        tryCatch({
            qf(1-alpha,df1,df2)
        }, warning = function(w) {
            stop("Sample size calculation failed.")
        }, error = function(e) {
            stop("Sample size calculation failed.")
        })

        fcrit <- qf(1-alpha, df1, df2)
        powerest <- 1-pf(fcrit, df1, df1, ncp=nc) ## Power Estimate

        ## Modified bisection
        if (powerest > power) {
            increment <- increment/2
            N <- N-increment
        }

        else if (powerest < power) {
            N <- N + increment ##Increment the N estimate by increment until it goes above the original estimate
        }

        if (abs(powerest - power) < tol) {
            found <- TRUE
        }

        count <- count + 1
    }

    Nlow <- floor(N)
    Nhigh <- ceiling(N)


    ## Calculate power using the lower N and the higher N
    Power <- dget("Power.R")
    powerlow <- Power(Nlow,k,rho,alpha)[1,1]
    powerhigh <- Power(Nhigh,k,rho,alpha)[1,1]


    ## Format output table
    output.table <- matrix(c(powerlow, powerhigh), nrow=1, ncol=2)
    rowname1 <- paste0("N = ", Nlow)
    rowname2 <- paste0("N = ", Nhigh)
    colnames(output.table) <- c(rowname1, rowname2)
    rownames(output.table) <- "<b>Power</b>"

    return(output.table)
}
