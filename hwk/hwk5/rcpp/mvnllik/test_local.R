rm(list = ls())
load("data.RData")
Rcpp::sourceCpp("logliks_Rcpp.cpp")
cores <- parallel::detectCores()
ll_rcpp_serial_time <- system.time({
    ll_rcpp_serial <- logliks_Rcpp(Y, D, thetas, .Machine$double.eps)
})
ll_rcpp_omp_time <- system.time({
    ll_rcpp_omp <- logliks_Rcpp_omp(Y, D, thetas, .Machine$double.eps, cores)
})
save(ll_rcpp_serial_time, ll_rcpp_serial, 
     ll_rcpp_omp_time, ll_rcpp_omp,
     file = "local_rcpp.RData")
