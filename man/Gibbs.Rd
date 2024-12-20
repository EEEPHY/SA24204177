% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hw_gibbs.R
\name{Gibbs}
\alias{Gibbs}
\title{Gibbs Sampler for Binomial-Beta Model}
\usage{
Gibbs(n, a, b, N, out)
}
\arguments{
\item{n}{The number of trials in the Binomial distribution.}

\item{a}{Shape parameters of the Beta prior distribution.}

\item{b}{Shape parameters of the Beta prior distribution.}

\item{N}{The total number of samples to generate.}

\item{out}{The number of initial "burn-in" iterations to discard.}
}
\value{
A NumericMatrix of dimension (N, 2), where each row contains:
Column 1: A sample of x (the number of successes in the Binomial distribution).
Column 2: A sample of y (the probability parameter in the Beta distribution).
}
\description{
This function implements a Gibbs sampling algorithm for a Bayesian model
where the prior for the probability parameter of a Binomial distribution
is a Beta distribution.
}
\examples{
Gibbs(10,3,5,1000,200)
}
