source("mvnllik.R")

thetas <- seq(0.1, 3, length = 100)

X <- runif(300, -1, 1)
D <- as.matrix(dist(matrix(X, ncol = 1), diag = TRUE, upper = TRUE))
Sigma <- exp(-D) + diag(sqrt(.Machine$double.eps), nrow(D))
library(mvtnorm)
Y <- rmvnorm(10000, sigma = Sigma)

# run 
ll <- logliks(Y, D, thetas, verb = 1)
