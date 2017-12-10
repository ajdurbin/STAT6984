nr=15000
x=matrix(rnorm(nr*nr), nrow=nr)
system.time(x %*% x)
