## -----------------------------------------------------------------------------

set.seed(123)
n <- 200
x1 <- rnorm(n, mean = 5, sd = 2)#服从正态分布
x2 <- runif(n, min = 0, max = 10)#服从均匀分布
x3 <- rexp(n, rate = 1)#服从指数分布
data_full <- as.matrix(data.frame(x1 = x1, x2 = x2, x3 = x3))
data_with_na<-data_full
data_with_na[sample(1:(3*n), 3*n*0.1)] <- NA #以0.1的缺失率对原始数据做随机缺失处理

## -----------------------------------------------------------------------------
library(SA24204177)
print(compare_imputation(data_with_na))

## -----------------------------------------------------------------------------

rmse<-function(A_full,A_imputed){
  squared_diffs <- (A_full - A_imputed)^2
  mean_squared_diff <- mean(squared_diffs)
  rmse <- sqrt(mean_squared_diff)
  return(rmse)
}
  em_imputed <- as.matrix(em_impute(data_with_na))
  rf_imputed <- as.matrix(random_forest_impute(data_with_na))
  svd_imputed <- as.matrix(svd_impute(data_with_na))
  knn_imputed <- as.matrix(knn_impute(data_with_na))

list(
  EM=rmse(data_full,em_imputed),
  randomForest=rmse(data_full,rf_imputed),
  SVD=rmse(data_full,svd_imputed),
  KNN=rmse(data_full,knn_imputed)
)


