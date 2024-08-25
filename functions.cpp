#include <RcppArmadillo.h>
#include <RcppEigen.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo, RcppEigen)]]

// [[Rcpp::export]]
Eigen::MatrixXd tcrossprod_cpp(const Eigen::MatrixXd& A, const Eigen::MatrixXd& B) {
  return A * B.transpose();
}

// [[Rcpp::export]]
double grad_cpp(const arma::mat& Qinv, const arma::mat& QinvR, const arma::mat& trQinv, const arma::mat& C, const double b) {
  return -0.5 * (arma::sum(arma::dot(Qinv,C)) - arma::sum(arma::dot(trQinv, C * QinvR))) * b;
}

/// [[Rcpp::export]]
Rcpp::List loglikelihood_cpp(const arma::colvec& series, const arma::mat& M) {
  int n = series.size();
  arma::mat Qinv = arma::inv(M);
  arma::mat QinvR = Qinv * series;
  arma::mat ll = -0.5 * (n * log(2*M_PI)) + log(arma::det(M)) + (series.t() * QinvR);
  return Rcpp::List::create(ll, Qinv, QinvR);
}

/// [[Rcpp::export]]
SEXP crossprod(const Eigen::Map<Eigen::MatrixXd> A, Eigen::Map<Eigen::MatrixXd> B) {
  Eigen::MatrixXd C = A * B;
  return Rcpp::wrap(C);
}

/// [[Rcpp::export]]
arma::mat minv_cpp(const arma::mat& x) {
  return arma::inv(x);
}