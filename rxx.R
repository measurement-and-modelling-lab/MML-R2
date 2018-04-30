function (data, X, Y) {

	tablegen <- dget("tablegen.r")
	
	if(nrow(data) != ncol(data)){
	  data <- cor(data)
	}

	Rxy <- data[X,Y,drop=FALSE] # single column matrix of the the correlations between Y and each predictor
	Rxx <- data[X,X] # matrix of the correlations between the predictors
	b <- solve(Rxx) %*% Rxy
	b <- round(b, 3)
	R2 <- t(b) %*% Rxy
	#R2 <- round(R2, 3)
	R2 <- cbind('R<sup>2</sup>', R2)

	cat('<center><div style="line-height: 175%;"><b>Squared Multiple Correlation</b></div>')
	tablegen(R2, TRUE)
    cat('<i>Y = ', Y, ', X = ', paste(X, collapse=', '), '</i></center>', sep="")
}
