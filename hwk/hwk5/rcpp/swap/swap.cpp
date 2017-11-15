#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void swap_Rcpp(IntegerVector v, int i, int j){
  
  int tmp;
  tmp = v[i];
  v[i] = v[j];
  v[j] = tmp;
  
}
