library(mvtnorm)

## loglik:
##
## evaluate log likelihood of theta definining the covariance structure
## using a MVN library function to calculate density
##

loglik <- function(Y, D, theta) 
  {
    if(ncol(Y) != nrow(D)) stop(" dimension mismatch")

    Sigma <- exp(-D/theta) + diag(sqrt(.Machine$double.eps), nrow(D))
    return(sum(dmvnorm(Y, sigma=Sigma, log=TRUE)))
  }


## loglik2:
##
## evaluate log likelihood of theta definining the covariance structure
## using a hand-written density evaluation in R
##

loglik2 <- function(Y, D, theta)
  {
    m <- nrow(D)
  	if(ncol(Y) != m) stop(" dimension mismatch")

  	Sigma <- exp(-D/theta) + diag(sqrt(.Machine$double.eps), nrow(D))
  	Schol <- chol(Sigma)
  	ldet <- 2*sum(log(diag(Schol)))
  	Si <- chol2inv(Schol)
    n <- nrow(Y)
  	ll <- -0.5*n*(m*log(2*pi) + ldet)
    for(i in 1:nrow(Y)) 
      ll <- ll - 0.5 * t(Y[i,]) %*% Si %*% Y[i,]
    return(ll)
  }


## logliks.R
## 
## wrapper function around log likelihood evaluation (by one of the above
## two functions), iterating over each theta value

logliks.R <- function(Y, D, thetas, verb=0, ll=loglik) 
  {
  	llik <- rep(NA, length(thetas))
  	for(t in 1:length(thetas)) {
  		llik[t] <- ll(Y, D, thetas[t])
  		if(verb > 0 &&  t %% verb == 0) cat("t=", t, ", ll=", llik[t], "\n", sep="")
  	}
  	return(llik)
  }


# logliks
# R interface to C version
logliks <- function(Y, D, thetas, verb = 0){

    m <- ncol(Y)
    n <- nrow(Y)
    tlen <- length(thetas)
    out <- rep(0.01, tlen)
    
    # insert object checks later

    ret <- .C("logliks_R",
              n = as.integer(n),
              m = as.integer(m), 
              Y = as.double(t(Y)),
              D = as.double(t(D)),
              thetas = as.double(thetas),
              tlen = as.integer(tlen),
              verb = as.integer(verb),
              out = as.double(out),
              DUP = FALSE)

     return(ret$out)

}

# load shared object
dyn.load("clect.so")

