// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericMatrix Gibbs_rcpp(int n,int a,int b,int num_samples,int out){
  NumericMatrix samples(num_samples,2);
  int x=0;
  double y=0.5;
  RNGScope scope;
  for (int i = 0; i < num_samples + out; i++) {
    x = R::rbinom(n, y); // Binomial(n, y)
    y = R::rbeta(x + a, n - x + b); // Beta(x + a, n - x + b)
    if (i >= out) {
      samples(i - out, 0) = x;
      samples(i - out, 1) = y;
    }
  }
  return samples;
}
