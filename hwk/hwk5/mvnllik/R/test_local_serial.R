load("data.RData")
source("mvnllik.R")

# run without accelerated linear algebra
ll_1_time <- system.time(ll_1 <- logliks.R(
  Y, D, thetas, verb = 0, ll = loglik))
ll_2_time <- system.time(ll_2 <- logliks.R(
  Y, D, thetas, verb = 0, ll = loglik2))
ll_c_serial_time <- system.time(
  ll_c_serial <- logliks(Y, D, thetas, verb = 0))

save(ll_1_time, ll_1, ll_2_time, ll_2, ll_c_serial_time, ll_c_serial,
     file = "local_serial.RData")
