function ( blah ) {
	tablegen <- dget("tablegen.r")

	data <- matrix(c(1,0.2,0.3,
					 0.2,1,0.4,
					 0.3,0.4,1), nrow=3, ncol=3)

	Y <- 2 # criterion
	X <- c(1,3) # predictors
	Rxy <- data[-Y,Y, drop=FALSE] # vector of the the correlations between Y and each predictor
	Rxx <- data[X,X] # matrix of the correlations between the predictors
	b <- solve(Rxx) %*% Rxy

	printfunction <- function () {

		cat('<div style="line-height: 175%;"><b>Rxx</b>\n\n</div>')
		tablegen(Rxx, FALSE)

		cat('<br><div style="line-height: 175%;"><b>Rxy</b>\n\n</div>')
		tablegen(Rxy, FALSE)

		cat('<br><div style="line-height: 175%;"><b>b*</b>\n\n</div>')
		tablegen(b, FALSE)

	}
	x <- capture.output(printfunction())

	cat(paste(x))

}
