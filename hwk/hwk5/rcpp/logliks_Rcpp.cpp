#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec logliks_Rcpp(arma::mat Y, arma::mat D, arma::vec thetas,
    double precision){
   
  int tlen = thetas.n_elem;
  int m = D.n_rows;
  int n = Y.n_rows;
  arma::vec likes(tlen);
  for(int i = 0; i < tlen; i++){
    double theta = thetas.elem(i);
    arma::mat Sigma = exp(-D/theta);
    arma::mat Schol = arma::chol(Sigma);
    double ldet = 2 * arma::accu(arma::log(Schol.diag()));
    arma::mat Si = arma::inv(Schol);
    double ll = -0.5 * n * (m * log(2 * arma::datum::pi) + ldet); 
    for(int j = 0; j < n; j++){
        ll -= arma::as_scalar(0.5 * Y.row(i) * Si * arma::trans(Y.row(i)));
    }
    likes.elem(i) = ll;;
  }  
  return(likes);
}

/*** R
thetas <- seq(0.1, 3, length=100)
X <- runif(300, -1, 1)
D <- as.matrix(dist(matrix(X, ncol=1), diag=TRUE, upper=TRUE))
Sigma <- exp(-D) + diag(sqrt(.Machine$double.eps), nrow(D))
library(mvtnorm)
Y <- rmvnorm(10000, sigma=Sigma)
print("before")
test <- logliks_Rcpp(Y, D, thetas, .Machine$double.eps)
print(test)
print(class(test))
print(dim(test))
print("after")
*/
