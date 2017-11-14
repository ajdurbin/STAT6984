load("data.RData")
Rcpp::sourceCpp("logliks_Rcpp.cpp")
ll_rcpp_time_mkl <- system.time({
    ll_rcpp_mkl <- logliks_Rcpp(Y, D, thetas, .Machine$double.eps)
})
save(ll_rcpp_time_mkl, ll_rcpp_mkl, file = "remote_rcpp.RData")
