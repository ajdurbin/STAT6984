// #include <Rcpp.h>
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
void swap_Rcpp(IntegerVector& v, int i, int j){
  
  int tmp;
  tmp = v[i];
  v[i] = v[j];
  v[j] = tmp;
  
}

/*** R

# v <- 1:1000000000
v <- 1:1000000
print(v[1])
print(v[2])
# recall that cpp indicies start at 0
system.time(swap_Rcpp(v, 0, 1))
print(v[1])
print(v[2])

rm(v)
# v <- 1:1000000000
v <- 1:1000000
swap.eval <- function()
{
  e <- quote({tmp <- v[i]; v[i] <- v[j]; v[j] <- tmp})
  eval(e, envir=parent.frame())
}
i <- 1
j <- 2
print(v[1])
print(v[2])
system.time(swap.eval())
print(v[1])
print(v[2])

*/

