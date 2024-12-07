#' Impute Missing Values Using k-Nearest Neighbors (kNN)
#'
#' This function uses the kNN() function from the VIM package to impute missing values in a given matrix.
#'
#' @param data A matrix or data frame containing missing values (NAs) to be imputed.
#' @param k The number of nearest neighbors to consider for imputation. Default is 5.
#'
#' @return A matrix with missing values imputed using the kNN algorithm.
#' @importFrom VIM kNN
#' @examples
#' # Create a sample matrix with missing values
#' mat <- matrix(c(1, 2, NA, 4, 5, NA, 7, 8, 9), nrow = 3, byrow = TRUE)
#' imputed_matrix <- knn_impute(mat, k = 3)
#' print(imputed_matrix)
#'
#' @export
knn_impute <- function(data, k = 3) {
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }
  imputed_data <- VIM::kNN(data, k = k)
  imputed_data <- imputed_data[, colnames(data), drop = FALSE]

  if (is.matrix(data)) {
    imputed_data <- as.matrix(imputed_data)
  }

  return(imputed_data)
}

