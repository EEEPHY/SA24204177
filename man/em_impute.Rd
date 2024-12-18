% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/em_impute.R
\name{em_impute}
\alias{em_impute}
\title{EM-based Missing Value Imputation using Rcpp}
\usage{
em_impute(data, max_iter = 100, tol = 1e-06)
}
\arguments{
\item{data}{A data frame or matrix with missing values (NA).}

\item{max_iter}{Maximum number of iterations for the EM algorithm. Default is 100.}

\item{tol}{Tolerance for convergence. Default is 1e-6.}
}
\value{
A complete data frame or matrix with missing values imputed.
}
\description{
This function imputes missing values in a dataset using the Expectation-Maximization (EM) algorithm.
}
\examples{
data <- t(matrix(c(1, 2, NA, 4, 5, NA, 7, 8, 9), nrow = 3, byrow = FALSE))
em_impute(data, max_iter = 50, tol = 1e-5)
}
