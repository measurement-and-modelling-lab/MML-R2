function (k, rho, alpha, power.desired) {
    ## k is the number of variables, including the predictor
    ## rho is the population correlation value
    ## alpha is the type I error rate
    ## power is the desired power

    ## Import functions
    Power <- dget("Power.R")
    SensibleRounding <- dget("SensibleRounding.R")
    
    ## Define key values
    N <- 1000             ## Starting point
    increment <- 1000     ## Special value used for bisection range finding
    tol <- 0.00000001     ## Tolerance
    power.difference <- 1 ## Initialize greater than tolerance
    iteration <- 0

    ## Increment N and calculate power until N gives a power very close to the desired one
    while(power.difference > tol){
        
        iteration <- iteration + 1
        if (iteration > 1000) {
            stop("Sample size calculation failed")
        }

        power.current <- Power(N, k, rho, alpha, FALSE)

        ## Modified bisection
        if (power.current > power.desired) {
            increment <- increment/2
            N <- N - increment
        } else if (power.current < power.desired) {
            N <- N + increment ## Increment the N estimate by increment until it goes above the original estimate
        } else {
            break
        }

        power.difference <- abs(power.current - power.desired)

    }

    ## Round N (which is almost certainly not an integer) both up and down
    N.lower <- floor(N)
    N.upper <- ceiling(N)

    ## Calculate power for each N; should straddle power.desired
    power.lower <- Power(N.lower, k, rho, alpha, FALSE)
    power.upper <- Power(N.upper, k, rho, alpha, FALSE)

    ## Assemble output matrix
    output.table <- matrix(c(power.lower, power.upper), nrow=1, ncol=2)
    rowname1 <- paste0("N = ", N.lower)
    rowname2 <- paste0("N = ", N.upper)
    colnames(output.table) <- c(rowname1, rowname2)
    rownames(output.table) <- "<b>Power</b>"

    ## Round output
    output.table <- SensibleRounding(output.table, 5)

    return(output.table)
}
