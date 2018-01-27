function (data, X, Y) {
  

  
  #function prototypes
	tablegen <- dget("rxx_tablegen.R")
	b_se <- dget("b_se.R")

	
	X <- as.integer(X)
	Y <- as.integer(Y)
	
	rawData <- data
	if(nrow(rawData) > ncol(data)){
	  #raw data is found
	  data <- cor(rawData)
	  
	  standardErrors <- b_se(rawData, X, Y)
	  #return()
	}
	
	
	Rxy <- data[X,Y,drop=FALSE] # single column matrix of the the correlations between Y and each predictor
	Rxx <- data[X,X] # matrix of the correlations between the predictors
	b <- solve(Rxx) %*% Rxy
	b <- round(b, 3)
	R2 <- t(b) %*% Rxy
	
	
	
	variables <- paste0('<b>X<sub>', c(1:nrow(data)), '</sub><sub>')
	data <- round(data, 3)
	data <- rbind(variables, data)
	data <- cbind(c('', variables), data)
	
	Rxx <- round(Rxx, 3)
	Rxx <- cbind(paste0('<b>X<sub>', X, '</sub></b>'), Rxx)
	Rxx <- rbind(c('', paste0('<b>X<sub>', X, '</sub></b>')), Rxx)
	
	Rxy <- round(Rxy, 3)
	Rxy <- cbind(paste0('<b>X<sub>', X, '</sub></b>'), Rxy)
	Rxy <- rbind(c('', paste0('<b>X<sub>', Y, '</sub></b>')), Rxy)
	b <- round(b, 3)
	b <- cbind(paste0('<b>X<sub>', X, '</sub></b>'), b)
	
	if(nrow(rawData) > ncol(data)){
	  standardErrors <- round(standardErrors, 3)
	  b <- cbind(b,standardErrors)
	}

	printfunction <- function () {
	    
	  cat('<div style="line-height: 175%;"><b>Input correlation matrix</b>\n\n</div>')
	  tablegen(data, TRUE)

		cat('<br><div style="line-height: 175%;"><b>R<sub>xx</sub></b>\n\n</div>')
		tablegen(Rxx, TRUE)

		cat('<br><div style="line-height: 175%;"><b>R<sub>xy</sub></b>\n\n</div>')
		tablegen(Rxy, TRUE)

		cat('<br><div style="line-height: 175%;"><b>b*</b>\n\n</div>')
		tablegen(b, TRUE)
		
		cat('<br><div style="line-height: 175%;"><b>R<sup>2</sup></b>\n\n</div>')
		tablegen(R2, TRUE)
		
    
	}
	x <- capture.output(printfunction())

	cat(paste(x))

}
