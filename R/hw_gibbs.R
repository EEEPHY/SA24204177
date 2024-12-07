#' Gibbs Sampler for Binomial-Beta Model
#'
#'This function implements a Gibbs sampling algorithm for a Bayesian model
#'where the prior for the probability parameter of a Binomial distribution
#'is a Beta distribution.
#'
#' @import Rcpp
#' @param n The number of trials in the Binomial distribution.
#' @param a Shape parameters of the Beta prior distribution.
#' @param b Shape parameters of the Beta prior distribution.
#' @param N The total number of samples to generate.
#' @param out The number of initial "burn-in" iterations to discard.
#' @return A NumericMatrix of dimension (N, 2), where each row contains:
#' Column 1: A sample of x (the number of successes in the Binomial distribution).
#' Column 2: A sample of y (the probability parameter in the Beta distribution).
#' @export
#' @examples
#' Gibbs(10,3,5,1000,200)

Gibbs <- function(n,a,b,N,out) {
  return(Gibbs_rcpp(n,a,b,N,out))
}
