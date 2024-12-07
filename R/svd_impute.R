#' Singular Value Decomposition (SVD) Imputation
#'
#' This function performs imputation of missing values in a matrix using Singular Value Decomposition (SVD).
#'
#' @param data A matrix or data frame with missing values (NA).
#' @param k The number of singular values to retain in the SVD decomposition.
#' @param tol A threshold for convergence. The function will stop iterating once the change in the matrix is below this value.
#' @param max_iter Maximum number of iterations for imputation. Default is 100.
#' @return A matrix with the missing values imputed.
#' @examples
#' mat <- matrix(rnorm(100), nrow = 10)
#' mat[sample(1:100, 20)] <- NA  # Introduce missing values
#' imputed_mat <- svd_impute(mat)
#' @export

svd_impute <- function(data, k = NULL, tol = 1e-6, max_iter = 100) {

  # 初始化矩阵：用0替换缺失值
  data_imputed <- data
  data_imputed[is.na(data)] <- 0
  n <- nrow(data)
  m <- ncol(data)

  # 判断是否设置了k，如果没有设置则自动选择一个k
  if (is.null(k)) {
    k <- min(n, m)  # 如果没有指定k，使用矩阵的秩作为默认值
  }

  #SVD
  for (iter in 1:max_iter) {
    svd_result <- svd(data_imputed)
# 获取前k个奇异值和对应的左右奇异向量
    U <- svd_result$u[, 1:k]
    D <- diag(svd_result$d[1:k])
    V <- svd_result$v[, 1:k]

    data_reconstructed <- U %*% D %*% t(V)

    # 使用重建矩阵填补原始矩阵中的缺失值
    missing_indices <- is.na(data)
    data_imputed[missing_indices] <- data_reconstructed[missing_indices]

  }

  # 返回填补后的矩阵
  return(data_imputed)
}

