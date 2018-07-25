function (k, rho, alpha, power.desired) {
    ## k is the number of variables, including the predictor
    ## rho is the population correlation value
    ## alpha is the type I error rate
    ## power is the desired power

    ## Import functions
    Power <- dget("Power.R")
    source("errorcheck.R")
    
    ## Check for errors
    areShort(k, rho, alpha, power.desired)
    areIntegers(k)
    areBetween0And1(rho, alpha, power.desired)
    if (k < 2) {
        stop("There must be at least two variables.")
    }

    ## Define key values
    N <- 1000             ## Starting point
    increment <- 1000     ## Special value used for bisection range finding
    tol <- 0.00000001     ## Tolerance
    power.difference <- 1 ## Initialize greater than tol
    iteration <- 0

    while(power.difference > tol){

        power.estimated <- Power(N, k, rho, alpha, FALSE)

        ## Modified bisection
        if (power.estimated > power.desired) {
            increment <- increment/2
            N <- N-increment
        } else if (power.estimated < power.desired) {
            N <- N + increment ## Increment the N estimate by increment until it goes above the original estimate
        } else {
            break
        }

        power.difference <- abs(power.estimated - power.desired)

        iteration <- iteration + 1
        if (iteration > 1000) {
            stop("Sample size calculation failed")
        }

    }

    ## Round N (which is almost certainly not an integer) both up and down
    N.low <- floor(N)
    N.high <- ceiling(N)

    ## Calculate power for each N; should straddle power.desired
    power.low <- Power(N.low,k,rho,alpha, FALSE)
    power.high <- Power(N.high,k,rho,alpha, FALSE)

    ## Assemble output matrix
    output.table <- matrix(c(power.low, power.high), nrow=1, ncol=2)
    rowname1 <- paste0("N = ", N.low)
    rowname2 <- paste0("N = ", N.high)
    colnames(output.table) <- c(rowname1, rowname2)
    rownames(output.table) <- "<b>Power</b>"

    ## Round output
    output.table <- round(output.table, 5)
    output.table[output.table == 1] <- "> 0.99999"
    output.table[output.table == 0] <- "< 0.00001"

    return(output.table)
}
