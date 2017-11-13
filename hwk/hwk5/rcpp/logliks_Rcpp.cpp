#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double logliks_Rcpp(arma::mat Y, arma::mat D, double theta,
    double precision){
   
  int m = D.n_rows;
  arma::mat Sigma;
  arma::mat tmp1(m,m);
  tmp1.eye();
  tmp1 = tmp1 * sqrt(precision);
  theta = 1 / theta;
  arma::mat tmp2 = -D*theta;
  Sigma = exp(tmp2) + tmp1;
  arma::mat Schol;
  Schol = arma::chol(Sigma);
  arma::mat diagschol;
  diagschol = arma::diagvec(Schol);
  log(diagschol);
  double ldet;
  ldet = arma::accu(diagschol);
  ldet = 2*ldet;
  arma::mat Si;
  Si = arma::inv(Schol);
  int n = Y.n_rows;
  double ll;
  ll = -0.5 * n * (m * log(2 * arma::datum::pi) + ldet);
  arma::mat row;
  row = Y.row(1);
  return(ll - 0.5 * row * Si * row.t());
   
}

/*** R
thetas <- seq(0.1, 3, length=100)
X <- runif(300, -1, 1)
D <- as.matrix(dist(matrix(X, ncol=1), diag=TRUE, upper=TRUE))
Sigma <- exp(-D) + diag(sqrt(.Machine$double.eps), nrow(D))
library(mvtnorm)
Y <- rmvnorm(10000, sigma=Sigma)
print("before")
test <- logliks_Rcpp(Y,D,thetas[1], .Machine$double.eps)
print(test)
print(class(test))
print(dim(test))
print("after")
*/
