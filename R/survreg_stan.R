#' Bayesian proportional hazards exponential model
#'
#' @export
#' @param X Design matrix
#' @param y Response times
#' @param v Censoring vector (0 = right-censored, 1 = exact time, 2 = left-censored, 3 = interval censored)
#' @param rcens If interval censored data, then the right-censoring time
#' @param ... Other arguments
#' @return An object of class `stanfit` returned by `rstan::sampling`
survreg_ph_exponential <- function(X, y, v, rcens = rep(NA, length(y)), sigma0 = 1, ...) {
  standata <- list(X = X, y = y, v = v, rcens = rcens, N = length(y), K = ncol(X), sigma0 = sigma0)
  out <- rstan::sampling(stanmodels$survreg_ph_exponential, data = standata, ...)
  return(out)
}

#' Bayesian proportional hazards weibull model
#'
#' @export
#' @param X Design matrix
#' @param y Response times
#' @param v Censoring vector (0 = right-censored, 1 = exact time, 2 = left-censored, 3 = interval censored)
#' @param rcens If interval censored data, then the right-censoring time
#' @param ... Other arguments
#' @return An object of class `stanfit` returned by `rstan::sampling`
survreg_ph_weibull <- function(X, y, v, rcens = rep(NA, length(y)), sigma0 = 1, gamma0 = 1/1000, ...) {
  standata <- list(X = X, y = y, v = v, rcens = rcens, N = length(y), K = ncol(X), sigma0 = sigma0, gamma0 = gamma0)
  out <- rstan::sampling(stanmodels$survreg_ph_weibull, data = standata, ...)
  return(out)
}
