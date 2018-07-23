function(data, N, criterion, predictors, familywise, confidence) {

    source("errorcheck.R")

    is.square <- ncol(data) == nrow(data)

    ## If the upper triangle of a correlation or covariance matrix is empty, make the matrix symmetric
    ## Otherwise, check whether the matrix is symmetric and if not return an error
    if (is.square) {
        current.upper.triangle <- data[upper.tri(data)]
        symmetric.upper.triangle <- t(data)[upper.tri(t(data))]
        if (all(is.na(current.upper.triangle))) {
            data[upper.tri(data)] <- symmetric.upper.triangle
        } else if (!all(current.upper.triangle == symmetric.upper.triangle)) {
            stop("Correlation or covariance matrix is not symmetric.")
        }
    }

    if (NA %in% as.numeric(data)) {
        stop("Your data has missing or non-numeric elements.")
    }

    if (!is.square) {
        N <- nrow(data)
        data <- cor(data)
    }

    areIntegers(N)
    areBetween0And1(confidence)
    if (criterion %in% predictors) {
        stop("A variable cannot be both a predictor and the criterion.")
    }
    if (length(predictors) < 2) {
        stop("You must have at least two predictors.")
    }
    

    ## Import other variables
    predictors <- as.numeric(predictors)
    criterion <- as.numeric(criterion)
    cx <- data[predictors, predictors]
    cxy <- data[predictors, criterion]
    vy <- data[criterion, criterion]
    alpha <- 1 - as.numeric(confidence)
    

    ## Extract the lower triangle and main diagonal from a matrix as a vector
    vech <- function(x) {
        t(x[!upper.tri(x)])
    }

    ## Transition of Duplicator Matrix
    Dn <- function(x) {
	mat <- diag(x)
	index <- seq(x * (x+1) / 2)
	mat[lower.tri(mat, TRUE)] <- index
	mat[upper.tri(mat)] <- t(mat)[upper.tri(mat)]
	outer(c(mat), index, function(x,y) ifelse(x == y, 1, 0))
    }

    ## Number of digits to round to in output
    digits <- 5

    ## Derivations
    scov <- rbind(cbind(cx, cxy), c(cxy, vy))
    p <- nrow(cx)

    ## Create covariance matrix of covariances under normality
    ## See Browne (1984) Eqn 4.6
    Kp.1ft <- solve(t(Dn(p + 1)) %*% Dn(p + 1)) %*% t(Dn(p + 1))
    cov.cov <- 2 * Kp.1ft %*% (scov %x% scov) %*% t(Kp.1ft)
    param <- c(vech(scov))
    ncovs <- length(param)

    ## Find the vector element numbers for the variances of X
    v.x.p1 <- c(1, rep(0, p - 1))
    for(i in 2:p) {
	v.x.p1[i] <- v.x.p1[i - 1] + p - (i - 2)
    }

    ## Store covariances and variances to use in the derivatives
    sx <- sqrt(diag(cx))
    sy <- sqrt(vy)

    bu <- solve(cx) %*% cxy
    ncx <- length(vech(cx))

    ## Derivatives of standardized regression coefficient wrt the covariances
    ## These are based on Yuan & Chan's (2011) equation 13
    db <- matrix(0, p, ncovs)
    V <- matrix(0, p, ncx)
    V[as.matrix(cbind(1:p, v.x.p1))] <- 1

    ##fouladi debug...first part 1 was my attempt to fix
    ##fouladi debug...second part 1 is using Jones/Waller looks as though it might be right
    ##this did not work PART1<-t(solve(diag(2 * sx * sy)) %*% (bu))
    PART1<-diag(c(solve(diag(2 * sx * sy)) %*% bu))
    PART2<-V

    ##fouladi debug ver 1 of PART3 puts it as in Yuan/Chan... note use of kronecker product
    ##fouladi debug ver 2 of PART3 adjusts it to be more similar to JOnes/Waller
    ##this did not work, using Yuan and Chan  this was off by magnitudes
    ##      PART3<-diag(sx / sy) %*% kronecker(t(bu),t(cx)) %*% Dn(p)
    ##following JOnes/Waller am using solve(cx) in a new variable called invcx
    invcx<-solve(cx)
    PART3<-diag(sx / sy) %*% kronecker(t(bu),t(invcx)) %*% Dn(p)

    ##fouladi debug
    ##original db[, 1:ncx] <- (diag(c(solve(diag(2 * sx * sy)) %*% bu)) %*% V -
    ##original 				diag(sx / sy) %*% (t(bu) %*% solve(cx)) %*% Dn(p))
    ##fouladifix line below by refering to original article Yuan and Chan 2011 with modification, see above for
    ## PART1 PART2 and PART3
    db[, 1:ncx] <- ((PART1 %*% PART2) -
                    PART3)
    ##fouladi debug
    ##original db[, (ncx+1):(ncx+p)] <- diag(sx / sy) %*% solve(cx)
    ##fouladifix line below below by refering to original article Yuan and Chan 2011 with modification, 
    ##using Yuan and Chan article magnitudes off 
    ##     db[, (ncx+1):(ncx+p)] <- diag(sx / sy) %*% t(cx)
    ##following Jones/Waller am using solve(cx) in a new variable called invcx
    db[, (ncx+1):(ncx+p)] <- diag(sx / sy) %*% t(invcx)

    db[,ncovs] <- -diag(sx / (2 * sy^3)) %*% bu

    ## Re-order the derivatives

    cx.nms <- matrix(0, p, p)
    cxy.nms <- c(rep(0, p), "var_y")

    for(i in 1:p) {
	for(j in 1:p) {
            cx.nms[i, j] <- paste("cov_x", i, "x", j, sep='')
	}
    }

    for(i in 1:p) {
	cxy.nms[i] <- paste("cov_x", i, "y", sep='')
    }

    old.ord <- c(vech(cx.nms), cxy.nms)
    new.ord <- vech(rbind(cbind(cx.nms, cxy.nms[1:p]), c(cxy.nms)))

    db <- db[, match(new.ord, old.ord)]

    ## Compute covariance matrix of standardized
    ## regression coefficient using the Delta Method
    DEL.cmat <- db %*% cov.cov %*% t(db) / (N - 3)
    b.nms <- NULL

    ##fouladi debug
    ## there was a typo, I changed patse to paste function
    for(i in 1:p) {
	b.nms[i] <- paste("beta_", i, sep='')
    }
    rownames(DEL.cmat) <- colnames(DEL.cmat) <- b.nms

    ## Compute standard errors of confidence intervals
    DELse <- sqrt(diag(DEL.cmat))
    CIs <- as.data.frame(matrix(0, p, 3))

    ## Calculate betas
    beta <- diag(sx) %*% bu * sy^-1

    ## Get the p values for beta = 0 for each beta
    test.statistic <- abs(beta / DELse)
    p.values <- pt(test.statistic, df=N-p-1, lower.tail=FALSE)*2

    beta.ranking <- rank(-abs(beta))
    number.of.predictors <- length(cxy)

    ## Generate the list of alphas for each method
    ## In the stepdown case this is assuming every p is less than alpha
    if (familywise == 'uncorrected') {
	alpha <- rep(alpha, number.of.predictors)
    } else if (familywise == 'bonferroni') {
	alpha.modifier <- rep(number.of.predictors, number.of.predictors)
	alpha <- alpha / alpha.modifier
    } else if (familywise == 'stepdown_bonferroni') {
	alpha <- alpha / (number.of.predictors - beta.ranking + 1)
    } else if (familywise == 'sidak') {
	alpha <- 1 - (1 - alpha)^(1 / number.of.predictors)
	alpha <- rep(alpha, number.of.predictors)
    } else { ## stepdown_sidak
	alpha <- 1 - (1 - alpha)^(1 / (number.of.predictors - (beta.ranking - 1)))
    }

    if (familywise %in% c('stepdown_bonferroni', 'stepdown_sidak')) {
        ## Extract the alphas that are greater than their respective p values
        ## If there are any, then there make them equal to the least among them
	comparison.matrix <- cbind(p.values, alpha)
        if (length(comparison.matrix[comparison.matrix[,1] > comparison.matrix[,2], 2]) > 0) {
            comparison.matrix[comparison.matrix[,1] > comparison.matrix[,2], 2] <- min(comparison.matrix[comparison.matrix[,1] > comparison.matrix[,2], 2])
            alpha <- comparison.matrix[,2]
        }

    }

    tc <- qt(alpha/2, N-p-1, lower = F)

    ## Error check on confidence interval
    if (FALSE %in% is.finite(tc)) {
        stop("Confidence interval calculation failed.")
    }

    for (i in 1:p) {
	CIs[i,] <- c(beta[i] - tc[i] * DELse[i], beta[i], beta[i] + tc[i] * DELse[i])
    }

    ## Assemble output
    output <- cbind(CIs, DELse, test.statistic, p.values, alpha)
    rownames(output) <- paste("<b>&beta;<sub>", 1:p, '</sub></b>', sep='')
    colnames(output) <- c("Lower", "Point", "Upper", "Std. Error", paste0("t<sub>", N-p-1, "</sub>"), "p", "&alpha;")


    ## Round output
    output <- round(output, 5)
    output[output==0] <- "< 0.00001"
    output[output==1] <- "> 0.99999"


    return(output)
}
