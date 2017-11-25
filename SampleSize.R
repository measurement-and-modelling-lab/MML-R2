function (k, rho, alpha, power) {

    k <- as.numeric(k)
    alpha <- as.numeric(alpha)
    rho <- as.numeric(rho)
    power <- as.numeric(power)
    df1 <- k - 1
    
    N = 1000
    i = 1
    increment = 1000 #Special value used for bijection range finding
    count = 0
    tol = 0.00000001 #Tolerance
    found = FALSE

    while(!found){

        df2 = N - df1 - 1
        Fobs <- (rho/df1)/((1-rho)/(N-df1-1))
        nc = N*(rho/(1-rho))

        result = tryCatch({
            qf(1-alpha,df1,df2)
        }, warning = function(w) {
            'problem'
        }, error = function(e) {
            'problem'
        }, finally = {
        })
        
        if ('problem' %in% result) {
            cat('<center><br><b><font color="red">Error: Sample size calculation failed.</font></b></center><center><i>k=',k,', &rho;<sup>2</sup>=',rho, ', &alpha;=',alpha,', 1-&beta;=',power,'</center></i>', sep = "")
            return(invisible(TRUE))
        }

        fcrit <- qf(1-alpha,df1,df2)
        powerest <- 1-pf(fcrit,df1,df1,ncp=nc) #Power Estimate
        #Moified bijection
        if (powerest > power) {
            increment <- increment/2  
            N <- N-increment
        }

        else if (powerest < power) {
            N <- N+increment #Increment the N estimate by increment until it goes above the Original estimate
        }

        if (abs(powerest-power) < tol) {
            found <- TRUE
        }

        count <- count +1;
    }

    Nlow <- floor(N)
    Nhigh <- ceiling(N)
	
	Power <- function(k, N, rho, alpha){
	
		df1 <- k
	
		df2 = N - df1 - 1

		Fobs <- (rho/df1)/((1-rho)/(N-df1-1))

		nc = N*(rho/(1-rho))
		fcrit <- qf(1-alpha,df1,df2) #Find Critical F value

		power <- round(1-pf(fcrit,df1,df1,ncp=nc), 5)
		if (power == 1) {
			power <- '> 0.99999'
		}
		return(power)
	}
	
    powerlow <- round(Power(df1,Nlow,rho,alpha), 5) # Calculates power using the lower N
    powerhigh <- round(Power(df1,Nhigh,rho,alpha), 5) # Calculates power using the higher N

    if (powerlow == 1) {
        powerlow <- '> 0.99999'
    }

    if (powerhigh == 1) {
        powerhigh <- '> 0.99999'
    }

    
  #Create formatted output
    
	output_table <- matrix(c(paste0('Power (N = ', Nlow, ')'), paste0('Power (N = ', Nhigh, ')'), powerlow, powerhigh), nrow=2, ncol=2)
	
    cat('<center><b>Sample Size Calculation Results</b>')
    tablegen <- dget("tablegen.r")
    tablegen(output_table,FALSE)
    cat('<i>k=',k,', &rho;<sup>2</sup>=',rho, ', &alpha;=',alpha,', 1-&beta;=',power,'</center></i>', sep = "")

}
