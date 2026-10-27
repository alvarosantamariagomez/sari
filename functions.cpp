#define ARMA_64BIT_WORD 1
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

// [[Rcpp::export]]
arma::mat conv_cpp(const arma::vec& a, const arma::vec& b) {
  int n = a.n_elem;
  arma::mat M(n, n);
  for (int j = 0; j < n; j++) M(0, j) = a(0) * b(j);
  for (int i = 1; i < n; i++) {
    M(i, 0) = a(i) * b(0);
    for (int j = 1; j < n; j++) M(i, j) = M(i-1, j-1) + a(i) * b(j);
  }
  return M;
}

// [[Rcpp::export]]
List cov_gaps_cpp(NumericVector d, NumericVector u, LogicalVector gaps, double log_s, bool deriv) {
  const int N = d.size();
  if (gaps.size() != N) stop("gaps and d have different lengths");
  if (deriv && u.size() != N) stop("u and d have different lengths");
  std::vector<int> rk(N);
  int n = 0;
  for (int i = 0; i < N; i++) { rk[i] = n; if (gaps[i]) n++; }
  NumericMatrix C(n, n), D(deriv ? n : 0, deriv ? n : 0);
  for (int l = 0; l < N; l++) {
    double s0 = 0.0, sl = 0.0, su = 0.0;
    for (int i = 0; i + l < N; i++) {
      const int j = i + l;
      s0 += d[i] * d[j];
      if (deriv) { sl += u[j] * d[i]; su += u[i] * d[j]; }
      if (gaps[i] && gaps[j]) {
        const int a = rk[j], b = rk[i];
        C(a, b) = s0; C(b, a) = s0;
        if (deriv) { const double v = -0.5 * log_s * s0 + sl + su; D(a, b) = v; D(b, a) = v; }
      }
    }
  }
  return List::create(C, D);
}