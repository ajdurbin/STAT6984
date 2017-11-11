load("data.RData")
source("mvnllik.R")

# run with accelerated linear algebra
ll_1_time_mkl <- system.time(ll_1_mkl <- logliks.R(
  Y, D, thetas, verb = 0, ll = loglik))
ll_2_time_mkl <- system.time(ll_2_mkl <- logliks.R(
  Y, D, thetas, verb = 0, ll = loglik2))
ll_c_serial_time_mkl <- system.time(
  ll_c_serial_mkl <- logliks(Y, D, thetas, verb = 0))

save(ll_1_time_mkl, ll_1_mkl, ll_2_time_mkl, ll_2_mkl, ll_c_serial_time_mkl,
     ll_c_serial_mkl, file = "remote_serial.RData")
