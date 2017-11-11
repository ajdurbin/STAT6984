load("data.RData")
source("mvnllik.R")
ll_c_omp_time_mkl <- system.time(
  ll_c_omp_mkl <- logliks(Y, D, thetas, verb = 0))
save(ll_c_omp_time_mkl, ll_c_omp_mkl, file = "remote_omp.RData")
