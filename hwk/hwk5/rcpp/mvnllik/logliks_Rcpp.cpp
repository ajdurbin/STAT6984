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
      double theta = thetas(i);
      arma::mat Sigma = exp(-D/theta);
      arma::mat Schol = arma::chol(Sigma);
      double ldet = 2 * arma::accu(arma::log(Schol.diag()));
      arma::mat Si = arma::inv_sympd(Sigma);
      double ll = -0.5 * n * (m * log(2 * arma::datum::pi) + ldet); 
      for(int j = 0; j < n; j++){
          ll -= arma::as_scalar(0.5 * Y.row(j) * Si * arma::trans(Y.row(j)));
      }
      likes(i) = ll;
    }  
    return(likes);
}


// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec logliks_Rcpp_omp(arma::mat Y, arma::mat D, arma::vec thetas,
    double precision, int cores){
    int tlen = thetas.n_elem;
    int m = D.n_rows;
    int n = Y.n_rows;
    arma::vec likes(tlen);
    omp_set_num_threads(cores);
    #pragma omp parallel for schedule(static)
    for(int i = 0; i < tlen; i++){
        double theta = thetas(i);
        arma::mat Sigma = exp(-D/theta);
        arma::mat Schol = arma::chol(Sigma);
        double ldet = 2 * arma::accu(arma::log(Schol.diag()));
        arma::mat Si = arma::inv_sympd(Sigma);
        double ll = -0.5 * n * (m * log(2 * arma::datum::pi) + ldet); 
        for(int j = 0; j < n; j++){
            ll -= arma::as_scalar(0.5 * Y.row(j) * Si * arma::trans(Y.row(j)));
        }
        likes(i) = ll;
    }     
    return(likes);
}