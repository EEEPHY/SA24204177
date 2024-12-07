#' Random Forest-based Missing Value Imputation
#'
#' This function imputes missing values using Random Forest, inspired by the missForest algorithm.
#'
#' @importFrom randomForest randomForest
#' @importFrom missForest missForest
#' @param data A data frame with missing values (NA).
#' @param ntree Number of trees in the forest. Default is 100.
#' @param max_iter Maximum number of iterations for imputation. Default is 10.
#' @return A complete data frame with missing values imputed.
#' @export
#' @examples
#' data <- data.frame(var1 = c(1, 2, NA, 4, 5),var2 = c(NA, 3, 4, NA, 6),var3 = c(1, NA, NA, 4, 5))
#' random_forest_impute(data, ntree = 50)

random_forest_impute <- function(data, ntree = 100, max_iter = 10) {
  #调用 missForest 插补
  imputed_data <- missForest::missForest(data, ntree = ntree, maxiter = max_iter)
  result <- as.data.frame(imputed_data$ximp)  # 确保结果为数据框
  if (!all(sapply(result, is.numeric))) {
    stop("random_forest_impute returned non-numeric values.")
  }
  return(result)
}


