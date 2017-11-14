load("data.RData")
Rcpp::sourceCpp("logliks_Rcpp.cpp")
ll_rcpp_time <- system.time({
    ll_rcpp <- logliks_Rcpp(Y, D, thetas, .Machine$double.eps)
})
save(ll_rcpp_time, ll_rcpp, file = "local_rcpp.RData")
