## ols.R:
##
## do ordinary least squares (linear) regression calculation
## for beta.hat

ols.R <- function(X, y, icept=TRUE, method=c("lm", "inv", "solve", "cp"))
{
	method <- match.arg(method)
	if(icept) X <- cbind(1, X)
	if(nrow(X) != length(y)) stop("dimension mismatch")
	if(method == "lm") beta <- drop(coef(lm(y~X-1)))
	else if(method == "inv") beta <- drop(solve(t(X) %*% X) %*% t(X) %*% y)
	else if(method == "solve") beta <- drop(solve(t(X) %*% X, t(X) %*% y))
	else beta <- drop(solve(crossprod(X),crossprod(X,Y)))
	return(beta)
}


dyn.load("clect.so")

## ols:
##
## faster C-version of ols.R

ols <- function(X, y, icept=TRUE, inv=FALSE)
{
	if(icept) X <- cbind(1, X)	
	m <- ncol(X)
	n <- nrow(X)
	if(n != length(y)) stop("dimension mismatch")

	ret <- .C("ols_R",
	  		  X = as.double(t(X)),
	  		  y = as.double(y),
	  		  n = as.integer(n),
	  		  m = as.integer(m),
	  		  inv = as.integer(inv),
	  		  beta.hat = double(m),
	  		  DUP = FALSE)

	return(ret$beta.hat)
}


## bootols.R:
##
## bootstrap sampling for the ordinary least squares
## (linear) regression calculation for beta.hat

bootols.R <- function(X, y, B=199, icept=TRUE, method=c("lm", "inv", "solve", "crossprod"))
{
	if(icept) X <- cbind(1, X)
	if(nrow(X) != length(y)) stop("dimension mismatch")

	beta <- matrix(NA, nrow=B, ncol=ncol(X))
	for(b in 1:B) {
		i <- sample(1:n, n, replace=TRUE)
		Xb <- X[i,]; yb <- Y[i]
		beta[b,] <- ols.R(Xb, yb, method=method, icept=FALSE)
	}

	return(beta)
}


## bootols:
##
## faster C-version of bootols.R

bootols <- function(X, y, B=199, icept=TRUE, inv=FALSE)
{
	if(icept) X <- cbind(1, X)	
	m <- ncol(X)
	n <- nrow(X)
	if(n != length(y)) stop("dimension mismatch")

	ret <- .C("bootols_R",
	  		  X = as.double(t(X)),
	  		  y = as.double(y),
	  		  n = as.integer(n),
	  		  m = as.integer(m),
	  		  B = as.integer(B),
	  		  inv = as.integer(inv),
	  		  beta.hat = double(m*B),
	  		  DUP = FALSE)

	## must be careful to return a transposed beta.hat matrix
	return(matrix(ret$beta.hat, nrow=B, byrow=TRUE))
}
