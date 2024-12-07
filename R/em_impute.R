#' EM-based Missing Value Imputation using Rcpp
#'
#' This function imputes missing values in a dataset using the Expectation-Maximization (EM) algorithm.
#'
#' @import RcppArmadillo
#' @param data A data frame or matrix with missing values (NA).
#' @param max_iter Maximum number of iterations for the EM algorithm. Default is 100.
#' @param tol Tolerance for convergence. Default is 1e-6.
#' @return A complete data frame or matrix with missing values imputed.
#' @export
#' @examples
#' data <- t(matrix(c(1, 2, NA, 4, 5, NA, 7, 8, 9), nrow = 3, byrow = FALSE))
#' em_impute(data, max_iter = 50, tol = 1e-5)

em_impute <- function(data, max_iter = 100, tol = 1e-6) {
  if (!is.matrix(data)) {
    data <- as.matrix(data)
  }
  data[is.na(data)] <- NaN
  result <- em_impute_rcpp(data, max_iter, tol)  # 调用Rcpp函数
  result <- as.matrix(result)
  if (!all(is.numeric(result))) {
    stop("em_impute_rcpp returned non-numeric values.")
  }
  as.data.frame(result)
}


