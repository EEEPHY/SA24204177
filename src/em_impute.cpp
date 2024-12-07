// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
#include <RcppArmadillo.h>
// [[Rcpp::export]]
arma::mat em_impute_rcpp(arma::mat data, int max_iter = 100, double tol = 1e-6) {

  arma::mat complete_data = data;

  // 1. 初始化缺失值填充为列均值
  for (arma::uword j = 0; j < data.n_cols; j++) {
    arma::uvec finite_indices = arma::find_finite(data.col(j));
    if (finite_indices.n_elem > 0) {
      arma::vec col_values = arma::vec(finite_indices.n_elem);
      for (arma::uword k = 0; k < finite_indices.n_elem; k++) {
        col_values(k) = data(finite_indices(k), j);
      }
      double col_mean = arma::mean(col_values);
      // 填充缺失值为均值
      arma::uvec missing_indices = arma::find_nonfinite(data.col(j));
      for (arma::uword m = 0; m < missing_indices.n_elem; m++) {
        complete_data(missing_indices(m), j) = col_mean;  // 用均值填充缺失值
      }
    }
  }

  // 2. EM算法主循环
  for (int iter = 0; iter < max_iter; iter++) {
    arma::mat old_data = complete_data;
    for (arma::uword i = 0; i < data.n_rows; i++) {
      arma::uvec observed = arma::find_finite(data.row(i));  //非缺失值
      arma::uvec missing = arma::find_nonfinite(data.row(i));  //缺失值

      if (missing.n_elem > 0) {
        arma::rowvec obs_values = arma::rowvec(observed.n_elem);
        for (arma::uword k = 0; k < observed.n_elem; k++) {
          obs_values(k) = complete_data(i, observed(k));
        }

        if (observed.n_elem < 2) {
          continue;
        }

        // 计算协方差矩阵
        arma::mat obs_cov = arma::mat(observed.n_elem, observed.n_elem, arma::fill::zeros);
        for (arma::uword p = 0; p < observed.n_elem; p++) {
          for (arma::uword q = 0; q < observed.n_elem; q++) {
            obs_cov(p, q) = complete_data(i, observed(p)) * complete_data(i, observed(q));
          }
        }
        //考虑到协方差矩阵可能非奇异，这里使用伪逆
        arma::mat obs_cov_inv;
          obs_cov_inv = arma::pinv(obs_cov);

        // 计算缺失值的预测
        arma::mat missing_obs_cov = arma::mat(missing.n_elem, observed.n_elem, arma::fill::zeros);
        for (arma::uword p = 0; p < missing.n_elem; p++) {
          for (arma::uword q = 0; q < observed.n_elem; q++) {
            missing_obs_cov(p, q) = complete_data(i, missing(p)) * complete_data(i, observed(q));  // 计算缺失值与非缺失值的协方差
          }
        }

        arma::colvec obs_values_col = obs_values.t();  // 转换为列向量

        // 计算预测的缺失值
        arma::rowvec predicted_missing = missing_obs_cov * obs_cov_inv * obs_values_col;
        for (arma::uword m = 0; m < missing.n_elem; m++) {
          complete_data(i, missing(m)) = predicted_missing(m);  // 用预测值填充缺失值
        }
      }
    }

    // 判断是否收敛
    if (arma::norm(complete_data - old_data, "fro") < tol) {
      break;
    }
  }

  return complete_data;
}
