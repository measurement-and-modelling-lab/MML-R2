function (N, k, RS, clevel) {

    ulimit <- 1-((1-clevel)/2)
    llimit <- (1-clevel)/2

    df1 <- k-1
    df2 <- N - df1 - 1
    adjRS <- 1- ((1-RS)*(N-1))/df2
    Fobs <- (RS/df1)/((1-RS)/(df2))
    adjRS = 1- ((1-RS)*(N-1))/df2
    nc <- N*(adjRS/(1-adjRS))

    tol <- 0.00000001

	
    # Lower limit
    FLL <- qf(llimit,df1,df2,ncp = nc,lower.tail = TRUE,log.p = FALSE) 
    RSLL <- (FLL/df2)/((1/df1)+(FLL/df2))
    ncLL <- N*(RSLL/(1-RSLL))
	
	if (is.na(abs((pf(Fobs,df1,df2,ncp=ncLL))-ulimit)> tol)) {
	    stop("Confidence interval calculation failed.")
	}
	
    while(abs((pf(Fobs,df1,df2,ncp=ncLL))-ulimit)> tol ) {
      Pfncll <- pf(Fobs,df1,df2,ncp=ncLL)
      ncLL <- -((Pfncll-ulimit)/(Pfncll -.5))*(ncLL-nc)+ncLL
	  if (ncLL < 0) {
	    ncLL <- 0
		break
	  }
    }
    RSLL <- round(ncLL/(ncLL+N), 5)


    # Upper limit
    FUL <- qf(ulimit,df1,df2,ncp = nc,lower.tail = TRUE,log.p = FALSE)
    RSUL <- (FUL/df2)/((1/df1)+(FUL/df2))
    ncUL <- N*(RSUL/(1-RSUL))
    while(abs((pf(Fobs,df1,df2,ncp=ncUL))-llimit)> tol ) {
      Pfncul <- pf(Fobs,df1,df2,ncp=ncUL)
      ncUL <- -((Pfncul-llimit)/(Pfncul -.5))*(ncUL-nc)+ncUL
	  if (ncUL < 0) {
	    cat('<center><br><b><font color="red">Error: Confidence interval calculation failed.</font></b></center><center><i>N=',N,', k=',k, ', R<sup>2</sup>=',RS,', CL=',clevel,'</center></i>', sep = "")
		return(invisible(TRUE))
	  }
    }
    RSUL <- round(ncUL/(ncUL+N), 5)

    # Lower bound
    clevel2 <- clevel - (1-clevel)
    ulimit <- 1-((1-clevel2)/2)
    llimit <- (1-clevel2)/2
    FLB <- qf(llimit,df1,df2,ncp = nc,lower.tail = TRUE,log.p = FALSE) 
    RSLB <- (FLB/df2)/((1/df1)+(FLB/df2))
    ncLB <- N*(RSLB/(1-RSLB))
    while(abs((pf(Fobs,df1,df2,ncp=ncLB))-ulimit)> tol ) {
      Pfnclb <- pf(Fobs,df1,df2,ncp=ncLB)
      ncLB <- -((Pfnclb-ulimit)/(Pfnclb -.5))*(ncLB-nc)+ncLB
	  if (ncLB < 0) {
	    ncLB <- 0
		break
	  }
    }
    RSLB <- round(ncLB/(ncLB+N), 5)

	plevel <- 1 - pf(Fobs, df1, N-df1-1, ncp=0)
	if (plevel < .00001) {
            plevel <- '< 0.00001'
    } else {
	    plevel <- round(plevel, 5)
		if (nchar(plevel) < 7) {
			plevel <- paste0(plevel,0)
		}
	}
	
	if (nchar(RSUL) < 7) {
		RSUL <- paste0(RSUL,0)
	}

	if (RSLL == 0) {
		RSLL <- '< 0.00001'
	} else if (nchar(RSLL) < 7) {
		RSLL <- paste0(RSLL,0)
	}

	if (RSLB == 0) {
		RSLB <- '< 0.00001'
	} else if (nchar(RSLB) < 7) {
		RSLB <- paste0(RSLB,0)
	}
	
output.table <- matrix(c(format(RSLL, scientific=F), format(RSUL, scientific=F), format(RSLB, scientific=F), format(plevel, scientific=F)), nrow=1, ncol=4)
colnames(output.table) <- c('Lower Limit', 'Upper Limit', 'Lower Bound', 'Plevel')
return(output.table)

## cat('<center><b>', 100*clevel, '% Confidence Interval (Fixed) Results</b>', sep="")
## tablegen(output_table,FALSE)
## cat('<i>N=',N,', k=',k, ', R<sup>2</sup>=',RS,', CL=',clevel,'</center></i>', sep="")

}
