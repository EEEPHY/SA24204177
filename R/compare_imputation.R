#' Compare Imputation Methods
#'
#' This function compares different imputation methods in terms of computational time.
#'
#' @import microbenchmark
#' @useDynLib SA24204177, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @param data A data frame with missing values.
#' @return A summary of the comparison.
#' @export
#' @examples
#' data <- data.frame(var1 = c(1, NA, 2, 4, 5),var2 = c(2, 3, 4, NA, 7),var3 = c(2, 3, 4, 5, NA))
#' compare_imputation(data)


compare_imputation <- function(data) {
  # 测量运行时间
  em_time <- microbenchmark::microbenchmark(em_impute(data))
  rf_time <- microbenchmark::microbenchmark(random_forest_impute(data))
  svd_time <- microbenchmark::microbenchmark(svd_impute(data))
  knn_time <- microbenchmark::microbenchmark(knn_impute(data))


  # 返回结果
    time_comparison = list(
      EM = summary(em_time)[,c(1,3,5,6)],
      SVD = summary(svd_time)[,c(1,3,5,6)],
      KNN = summary(knn_time)[,c(1,3,5,6)],
      RandomForest = summary(rf_time)[,c(1,3,5,6)]

    )
}


