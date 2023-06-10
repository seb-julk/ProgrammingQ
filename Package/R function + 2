// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace arma;
using namespace Rcpp;


// [[Rcpp::export]]
Rcpp::NumericVector cplus(NumericVector x, Function g) {
    NumericVector result = g(x);  // Call R function g with input x
    result = result + 2;  // Add 2 to the result
    
    return result;
}
