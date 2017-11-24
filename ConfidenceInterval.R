function (N, k, RS, clevel) {

    N <- as.numeric(N)
    k <- as.numeric(k)
    RS <- as.numeric(RS)
    clevel <- as.numeric(clevel)

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
	    cat('<center><br><b><font color="red">Error: Confidence interval calculation failed.</font></b></center><center><i>N=',N,', k=',k, ', R<sup>2</sup>=',RS,', CL=',clevel,'</center></i>', sep = "")
		return(invisible(TRUE))
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
	
	#cat('<center><b>', 100*clevel, '% Confidence Interval Results</b></center><center><table border="1"><tr><td>&nbsp;&nbsp;Lower Limit&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br></td><td>&nbsp;&nbsp;',format(RSLL, scientific=F), '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td></tr><tr><td>&nbsp;&nbsp;Upper Limit<br></td><td>&nbsp;&nbsp;', RSUL, '</td></tr><tr><td>&nbsp;&nbsp;Lower Bound<br></td><td>&nbsp;&nbsp;', format(RSLB, scientific=F),'</td></tr> <tr><td>&nbsp;&nbsp;Plevel</td><td>&nbsp;&nbsp;', format(plevel, scientific=F), '</td></tr></table></center><center><i>N=',N,', k=',k, ', R<sup>2</sup>=',RS,', CL=',clevel,'</center></i>', sep="")



output_table <- matrix(c('Lower Limit', 'Upper Limit', 'Lower Bound', 'Plevel', format(RSLL, scientific=F),format(RSUL, scientific=F),format(RSLB, scientific=F),format(plevel, scientific=F)), nrow=4, ncol=2)

tablegen <- dget("tablegen.r")

cat('<center><b>', 100*clevel, '% Confidence Interval (Fixed) Results</b>', sep="")
tablegen(output_table,FALSE)
cat('<i>N=',N,', k=',k, ', R<sup>2</sup>=',RS,', CL=',clevel,'</center></i>', sep="")

	#cat('<center><b>', 100*clevel, '% Confidence Interval (Fixed) Results</b></center><center><table border="1"><tr"><td>&nbsp;&nbsp;Lower Limit&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br></td><td align="right">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;',format(RSLL, scientific=F), '&nbsp;&nbsp;</td></tr><tr ><td>&nbsp;&nbsp;Upper Limit<br></td><td  align="right">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;', RSUL, '&nbsp;&nbsp;</td></tr><tr><td>&nbsp;&nbsp;Lower Bound<br></td><td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;', format(RSLB, scientific=F),'&nbsp;&nbsp;</td></tr><tr><td>&nbsp;&nbsp;Plevel</td><td align="right">', format(plevel, scientific=F), '&nbsp;&nbsp;</td></tr></table></center><center><i>N=',N,', k=',k, ', R<sup>2</sup>=',RS,', CL=',clevel,'</center></i>', sep="")

}
