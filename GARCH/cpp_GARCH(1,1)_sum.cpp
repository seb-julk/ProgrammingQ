// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace arma;
using namespace Rcpp;

// We have 3 parameters here. It follows reexam 2021

//[[Rcpp::export]]
List CGarchSim(arma::vec vPar, Rcpp::Function fSim, int iT = 1, arma::vec vFpar = 0) { 
    double dOmega = vPar(0);
    double dAlpha = vPar(1);
    double dBeta = vPar(2);
    
    
    arma::vec vSigma2 = zeros(iT);
    arma::vec vY = zeros(iT);
    
    arma::vec vEps = zeros(iT);
    
    
    vSigma2(0) = dOmega / ( 1 - dAlpha - dBeta); 
    SEXP dEps0 = fSim(1,vFpar);
    vEps(0) = *REAL(dEps0);
    vY(0) =  vEps(0) * sqrt(vSigma2(0));
    
    
    for (int t = 1; t < iT; t++) {
        vSigma2(t) = dOmega + dAlpha * pow(vEps(t-1),2) + dBeta * vSigma2(t-1);
        SEXP dEps = fSim(1,vFpar);
        vEps(t) = *REAL(dEps);
        vY(t) =vEps(t) * sqrt(vSigma2(t));
    }
    
    
    return Rcpp::List::create(Rcpp::Named("vY") = vY,
                              Rcpp::Named("vSigma2") = vSigma2);
}
