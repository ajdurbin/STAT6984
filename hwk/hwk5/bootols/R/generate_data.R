n <- 5000
d <- 200
X <- 1/matrix(rt(n*d, df=1), ncol=d)
beta <- c(1:d, 0)
Y <- beta[1] + X %*% beta[-1] + rnorm(100, sd=3)
save.image(file = "data.RData")
