#' Bayesian binomial regression with Stan
#'
#' @export
#' @param X Numeric matrix of design.
#' @param y Numeric vector of responses.
#' @param n Numeric vector of number of trials.
#' @param mu0 Prior mean for regression coefficients.
#' @param Sigma0 Prior covariance for regression coefficients.
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
glm_binomial <- function(
  X, y, n, link = 0, mu0 = rep(0, ncol(X)), Sigma0 = diag(1, ncol(X)), method = "sampling", ...) {
  fun <- match.fun(method)
  standata <- list(X = X, y = y, n = n, N = length(y), K = ncol(X), mu0 = mu0, Sigma0 = Sigma0, link = link)
  out <- fun(stanmodels$glm_binomial, data = standata, ...)
  return(out)
}

#' Bayesian logistic binomial regression with Stan
#'
#' @export
#' @param X Numeric matrix of design.
#' @param y Numeric vector of responses.
#' @param n Numeric vector of number of trials.
#' @param mu0 Prior mean for regression coefficients.
#' @param Sigma0 Prior covariance for regression coefficients.
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
glm_binomial_logistic <- function(
  X, y, n, mu0 = rep(0, ncol(X)), Sigma0 = diag(1, ncol(X)), method = "sampling", ...) {
  fun <- match.fun(method)
  standata <- list(X = X, y = y, n = n, N = length(y), K = ncol(X), mu0 = mu0, Sigma0 = Sigma0)
  out <- fun(stanmodels$glm_binomial_logistic, data = standata, ...)
  return(out)
}

#' Bayesian probit binomial regression with Stan
#'
#' @export
#' @param X Numeric matrix of design.
#' @param y Numeric vector of responses.
#' @param n Numeric vector of number of trials.
#' @param mu0 Prior mean for regression coefficients.
#' @param Sigma0 Prior covariance for regression coefficients.
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
glm_binomial_probit <- function(
  X, y, n, mu0 = rep(0, ncol(X)), Sigma0 = diag(1, ncol(X)), method = "sampling", ...) {
  fun <- match.fun(method)
  standata <- list(X = X, y = y, n = n, N = length(y), K = ncol(X), mu = mu0, Sigma = Sigma0)
  out <- fun(stanmodels$glm_binomial_probit, data = standata, ...)
  return(out)
}

#' Bayesian logistic beta-binomial regression with Stan
#'
#' @export
#' @param X Numeric matrix of design.
#' @param y Numeric vector of responses.
#' @param n Numeric vector of number of trials.
#' @param mu0 Prior mean for regression coefficients.
#' @param Sigma0 Prior covariance for regression coefficients.
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
glm_betabinomial <- function(
  X, y, n, link = 0, mu0 = rep(0, ncol(X)), Sigma0 = diag(1, ncol(X)), method = "sampling", ...) {
  fun <- match.fun(method)
  standata <- list(X = X, y = y, n = n, N = length(y), K = ncol(X), mu = mu0, Sigma = Sigma0, link = link)
  out <- fun(stanmodels$glm_betabinomial, data = standata, ...)
  return(out)
}


#' Bayesian logistic regression with Stan
#'
#' @export
#' @param X Numeric matrix of design.
#' @param y Numeric vector of responses.
#' @param n Numeric vector of number of trials.
#' @param Xpred Numeric matrix of prediction.
#' @param npred Numeric vector of number of trials to predict.
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
logreg_stan_pred <- function(X, y, n, Xpred, npred, method = "sampling", ...) {
  fun <- match.fun(method)
  standata <- list(X = X, y = y, n = n, N = length(y), K = ncol(X), Npred = nrow(Xpred), Xpred = Xpred, npred = npred)
  out <- fun(stanmodels$log_reg_pred, data = standata, ...)
  return(out)
}
