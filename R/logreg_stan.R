#' Bayesian logistic regression with Stan
#'
#' @export
#' @param X Numeric matrix of design.
#' @param y Numeric vector of responses.
#' @param n Numeric vector of number of trials.
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
logreg_stan <- function(X, y, n, method = "sampling", ...) {
  fun <- match.fun(method)
  standata <- list(X = X, y = y, n = n, N = length(y), K = ncol(X))
  out <- fun(stanmodels$log_reg, data = standata, ...)
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
