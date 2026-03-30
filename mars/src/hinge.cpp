#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector h(double s, NumericVector x, double t) {
  int n = x.size();
  NumericVector out(n);
  for (int i = 0; i < n; i++) {
    double val = s * (x[i] - t);
    out[i] = val > 0 ? val : 0;
  }
  return out;
}
