rm(list = ls())
load("data.RData")
Rcpp::sourceCpp("logliks_Rcpp.cpp")
cores <- parallel::detectCores()
ll_rcpp_serial_time_mkl <- system.time({
    ll_rcpp_serial_mkl <- logliks_Rcpp(Y, D, thetas, .Machine$double.eps)
})
ll_rcpp_omp_time_mkl <- system.time({
    ll_rcpp_omp_mkl <- logliks_Rcpp_omp(Y, D, thetas, .Machine$double.eps, cores)
})
save(ll_rcpp_serial_time_mkl, ll_rcpp_serial_mkl, 
     ll_rcpp_omp_time_mkl, ll_rcpp_omp_mkl,
     file = "remote_rcpp.RData")