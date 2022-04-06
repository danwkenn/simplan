#' Constant distribution (dirac).
#' @param n Number of "draws".
#' @param location Location of spike/only value with non-zero probability.
#' @export
rconstant <- function(n,location){
  rep(location, n)
}

#' Uniform distribution
#' @param n Number of draws.
#' @param lower Lower limit.
#' @param upper Upper limit.
#' @export
runiform <- function(n, lower, upper){
  runif(n, lower, upper)
}

#' Multivariate normal:
#' @param n Number of draws.
#' @param mean vector mean.
#' @param covariance matrix for covariance.
#' @export
rmvnorm <- function(n, mean, covariance){
  MASS::mvrnorm(n,mean,covariance)
}

#' Sample from a finite set.
#' @param n Number of "draws".
#' @param locations Location of spikes/only values with non-zero probability.
#' @param frequencies Frequencies/probabilities of the spikes.
#' @export
rdiscrete <- function(n, locations, frequencies = NULL){
  sample(x = locations, size = n, replace = TRUE, prob = frequencies)
}

#' Sample a bootstrap statistic from a finite set.
#' @param n Number of "draws".
#' @param data Data vector to draw resamples.
#' @param statistic Statistic to run.
#' @export
rbootstrap <- function(n, data, statistic){
  sapply(1:n, function(i){x = sample(x = data, size = length(data), replace = TRUE);statistic(x)})
}


r_dists <- c(
  normal = "norm",
  gamma = "gamma",
  constant = "constant",
  mvnormal = "mvnorm",
  discrete = "discrete",
  bootstrap = "bootstrap",
  uniform = "uniform")

simplan_dists <- c("mvnorm", "bootstrap","discrete","uniform")


