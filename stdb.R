function(X = NULL, y = NULL,
				cov.x = NULL, cov.xy = NULL,
				var.y = NULL, Nobs = NULL,
				criterion = NULL, predictors = NULL,
				alpha = .05, digits = 3) {

tablegen <- dget("tablegen.r")

# vech function
vech <- function(x) t(x[!upper.tri(x)])

# Transition of Duplicator Matrix
Dn <- function(x){
	mat <- diag(x)
	index <- seq(x * (x+1) / 2)
	mat[lower.tri(mat, TRUE)] <- index
	mat[upper.tri(mat)] <- t(mat)[upper.tri(mat)]
	outer(c(mat), index, function(x,y) ifelse(x == y, 1, 0))
}

#########################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Error Checking~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
if(is.null(X) & !is.null(y))
	stop("\n y is not defined\n Need to specify both X and y\n")
if(!is.null(X) & is.null(y))
	stop("\n X is not defined\n Need to specify both X and y\n")
if(is.null(X) & is.null(y)) {
	if(is.null(cov.x) | is.null(cov.xy) | is.null(var.y) | is.null(Nobs))
		stop("\nYou need to specify covariances and sample size\n")
	scov <- rbind(cbind(cov.x, cov.xy), c(cov.xy, var.y))
	N <- Nobs
	p <- nrow(cov.x)
} else {
	scov <- cov(cbind(X, y))
	N <- length(y)
	p <- ncol(X)
}

# Create covariance matrix of covariances under normality
# See Browne (1984) Eqn 4.6

Kp.1ft <- solve(t(Dn(p + 1)) %*% Dn(p + 1)) %*% t(Dn(p + 1))
cov.cov <- 2 * Kp.1ft %*% (scov %x% scov) %*% t(Kp.1ft)
param <- c(vech(scov))
ncovs <- length(param)

# Find the vector element numbers for the variances of X
v.x.p1 <- c(1, rep(0, p - 1))
for(i in 2:p) v.x.p1[i] <- v.x.p1[i - 1] + p - (i - 2)

# Store covariances and variances to use in the derivatives

cx <- scov[1:p, 1:p]
cxy <- scov[1:p, p+1]
vy <- scov[p+1, p+1]

sx <- sqrt(diag(cx))
sy <- sqrt(vy)

bu <- solve(cx) %*% cxy
ncx <- length(vech(cx))

# Derivatives of standardized regression coefficient wrt the covariances
# These are based on Yuan & Chan's (2011) equation 13

db <- matrix(0, p, ncovs)
V <- matrix(0, p, ncx)
V[as.matrix(cbind(1:p, v.x.p1))] <- 1

#fouladi debug...first part 1 was my attempt to fix
#fouladi debug...second part 1 is using Jones/Waller looks as though it might be right
#this did not work PART1<-t(solve(diag(2 * sx * sy)) %*% (bu))
PART1<-diag(c(solve(diag(2 * sx * sy)) %*% bu))
PART2<-V
#fouladi debug ver 1 of PART3 puts it as in Yuan/Chan... note use of kronecker product
#fouladi debug ver 2 of PART3 adjusts it to be more similar to JOnes/Waller
#this did not work, using Yuan and Chan  this was off by magnitudes
#      PART3<-diag(sx / sy) %*% kronecker(t(bu),t(cx)) %*% Dn(p)
#following JOnes/Waller am using solve(cx) in a new variable called invcx
invcx<-solve(cx)
PART3<-diag(sx / sy) %*% kronecker(t(bu),t(invcx)) %*% Dn(p)


#fouladi degug
#original db[, 1:ncx] <- (diag(c(solve(diag(2 * sx * sy)) %*% bu)) %*% V -
#original 				diag(sx / sy) %*% (t(bu) %*% solve(cx)) %*% Dn(p))
#fouladifix line below by refering to original article Yuan and Chan 2011 with modification, see above for
# PART1 PART2 and PART3
db[, 1:ncx] <- ((PART1 %*% PART2) -
 				PART3)
#fouladi debug
#original db[, (ncx+1):(ncx+p)] <- diag(sx / sy) %*% solve(cx)
#fouladifix line below below by refering to original article Yuan and Chan 2011 with modification, 
#using Yuan and Chan article magnitudes off 
#     db[, (ncx+1):(ncx+p)] <- diag(sx / sy) %*% t(cx)
#following JOnes/Waller am using solve(cx) in a new variable called invcx
db[, (ncx+1):(ncx+p)] <- diag(sx / sy) %*% t(invcx)

db[,ncovs] <- -diag(sx / (2 * sy^3)) %*% bu

# Re-order the derivatives

cx.nms <- matrix(0, p, p)
cxy.nms <- c(rep(0, p), "var_y")

for(i in 1:p) for(j in 1:p) cx.nms[i, j] <- paste("cov_x", i, "x", j, sep='')
for(i in 1:p) cxy.nms[i] <- paste("cov_x", i, "y", sep='')

old.ord <- c(vech(cx.nms), cxy.nms)
new.ord <- vech(rbind(cbind(cx.nms, cxy.nms[1:p]), c(cxy.nms)))

db <- db[, match(new.ord, old.ord)]

# Compute covariance matrix of standardized
# regression coefficient using the Delta Method

DEL.cmat <- db %*% cov.cov %*% t(db) / (N - 3)
b.nms <- NULL

#fouladi debugg
# there was a typo, I changed patse to paste function
for(i in 1:p) b.nms[i] <- paste("beta_", i, sep='')
rownames(DEL.cmat) <- colnames(DEL.cmat) <- b.nms

# compute standard errors of confidence intervals

DELse <- sqrt(diag(DEL.cmat))
CIs <- as.data.frame(matrix(0, p, 3))
tc <- qt(alpha / 2, N-p-1, lower = F)
beta <- diag(sx) %*% bu * sy^-1
for(i in 1:p) CIs[i,] <- c(beta[i] - tc * DELse[i], beta[i], beta[i] + tc * DELse[i])
CIs <- round(as.matrix(CIs), 3)
CIs <- rbind(c("Lower Bound", "Estimate", "Upper Bound"), CIs)
CIs <- cbind(c('', paste("&beta;<sub>", predictors, "</sub>", sep='')), CIs)

cat('<center><b>', 100* (1 - alpha), '% CIs for Standardized Regression Coefficients:</b>', sep="")
tablegen(CIs, TRUE)
cat('<i>Y = ', criterion, ', X = ', paste(predictors, collapse=','), '</i></center>', sep="")
}
