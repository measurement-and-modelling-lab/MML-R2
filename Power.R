function(N, k, rho, alpha){

	N <- as.numeric(N)
	df1 <- as.numeric(k)-1
	rho <- as.numeric(rho)
	alpha <- as.numeric(alpha)

	df2 = N - df1 - 1
	Fobs <- (rho/df1)/((1-rho)/(N-df1-1))
	nc = N*(rho/(1-rho))
	
	if (is.na(qf(1-alpha,df1,df2))) {
	    cat('<center><br><b><font color="red">Error: Power calculation failed.</font></b></center><center><i>N=',N,', k=',k, ', &rho;<sup>2</sup>=',rho,', &alpha;=',alpha,'</center></i>', sep = "")
		return(invisible(TRUE))
	}
	
	fcrit <- qf(1-alpha,df1,df2)
	power <- round(1-pf(fcrit,df1,df1,ncp=nc), 5)
	if (power == 1) {
		power <- '> 0.99999'
	}



        output_table <- matrix(c('Power', power), nrow=1, ncol=2)

        cat('<center><b>Power Calculation Results</b>')
        tablegen <- dget("tablegen.r")
        tablegen(output_table,FALSE)
	cat('<i>N=',N,', k=',k, ', &rho;<sup>2</sup>=',rho,', &alpha;=',alpha,'</i></center>', sep="")

}
