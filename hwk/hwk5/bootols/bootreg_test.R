
source("bootreg.R")

## generate data - same as in mc.R but bigger
#n <- 10000
#d <- 500
#X <- 1/matrix(rt(n*d, df=1), ncol=d)
#beta <- c(1:d, 0)
#Y <- beta[1] + X %*% beta[-1] + rnorm(100, sd=3)

## testing single OLS
##system.time(fit.Rinv <- ols.R(X, Y, method="inv"))
##system.time(fit.Rlm <- ols.R(X, Y, method="lm"))
##system.time(fit.Rsolve <- ols.R(X, Y, method="solve"))
##system.time(fit.Rcp <- ols.R(X, Y, method="cp"))
##system.time(fit.Cinv <- ols(X, Y, inv=TRUE))
##system.time(fit.Csolve <- ols(X, Y))

## generate data - smaller for bootstrap
#n <- 5000
#d <- 200
#X <- 1/matrix(rt(n*d, df=1), ncol=d)
#beta <- c(1:d, 0)
#Y <- beta[1] + X %*% beta[-1] + rnorm(100, sd=3)

## testing bootstrapped OLS
##system.time(boot.Rlm <- bootols.R(X, Y, B=1000, method="lm"))
##system.time(boot.Rinv <- bootols.R(X, Y, B=1000, method="inv"))
##system.time(beta.Rsolve <- bootols.R(X, Y, B=1000, method="solve"))
##system.time(beta.Rcp <- bootols.R(X, Y, B=1000, method="cp"))
##system.time(boot.Cinv <- bootols(X, Y, B=1000, inv=TRUE))
##system.time(beta.Csolve <- bootols(X, Y, B=1000))

# alex's test
n <- 5000
d <- 200
X <- 1/matrix(rt(n*d, df=1), ncol=d)
beta <- c(1:d, 0)
Y <- beta[1] + X %*% beta[-1] + rnorm(100, sd=3)
system.time(beta.Rsolve <- bootols.R(X, Y, B=1000, method="solve"))

