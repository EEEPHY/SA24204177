# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

em_impute_rcpp <- function(data, max_iter = 100L, tol = 1e-6) {
    .Call(`_SA24204177_em_impute_rcpp`, data, max_iter, tol)
}

Gibbs_rcpp <- function(n, a, b, num_samples, out) {
    .Call(`_SA24204177_Gibbs_rcpp`, n, a, b, num_samples, out)
}

