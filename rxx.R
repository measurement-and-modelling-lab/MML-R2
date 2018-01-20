function (data, X, Y) {

	tablegen <- dget("rxx_tablegen.R")

	X <- as.integer(X)
	Y <- as.integer(Y)
	Rxy <- data[X,Y,drop=FALSE] # single column matrix of the the correlations between Y and each predictor
	Rxx <- data[X,X] # matrix of the correlations between the predictors
	b <- solve(Rxx) %*% Rxy
	b <- round(b, 3)
	R2 <- t(b) %*% Rxy
	
	variables <- paste0('<b>X<sub>', c(1:nrow(data)), '</sub><sub>')
	data <- rbind(variables, data)
	data <- cbind(c('', variables), data)
	
	Rxx <- cbind(paste0('<b>X<sub>', X, '</sub></b>'), Rxx)
	Rxx <- rbind(c('', paste0('<b>X<sub>', X, '</sub></b>')), Rxx)
	
	Rxy <- cbind(paste0('<b>X<sub>', X, '</sub></b>'), Rxy)
	Rxy <- rbind(c('', paste0('<b>X<sub>', Y, '</sub></b>')), Rxy)
	b <- cbind(paste0('<b>X<sub>', X, '</sub></b>'), b)

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
