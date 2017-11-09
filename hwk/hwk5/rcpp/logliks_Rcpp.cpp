#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec logliks_Rcpp(arma::mat Y,arma::mat D, double theta){
   
  int m = D.n_rows;
  arma::mat Sigma;
  arma::mat tmp1(m,m);
  tmp1.eye();
  tmp1 = tmp1 * sqrt(0.0000000001);
  theta = 1 / theta;
  arma::mat tmp2 = -D*theta;
  Sigma = exp(tmp2) + tmp1;
  arma::mat Schol;
  Schol = arma::chol(Sigma);
  arma::vec ldet;
  ldet = 2*arma::sum(log(diagmat(Schol)));
  return(ldet);
   
}

/*** R
thetas <- seq(0.1, 3, length=100)
X <- runif(300, -1, 1)
D <- as.matrix(dist(matrix(X, ncol=1), diag=TRUE, upper=TRUE))
Sigma <- exp(-D) + diag(sqrt(.Machine$double.eps), nrow(D))
library(mvtnorm)
Y <- rmvnorm(10000, sigma=Sigma)
print("before")
test <- logliks_Rcpp(Y,D,thetas[1])
print(test)
print(class(test))
print(dim(test))
print("after")
*/
