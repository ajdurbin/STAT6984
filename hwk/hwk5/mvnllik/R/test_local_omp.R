load("data.RData")
source("mvnllik.R")
ll_c_omp_time <- system.time(ll_c_omp <- logliks(Y, D, thetas, verb = 0))
save(ll_c_omp_time, ll_c_omp, file = "local_omp.RData")
