#' Bayesian lasso regression with Stan
#'
#'@export
#' @param X Design matrix (no intercept)
#' @param y Response vector
#' @param ... Additional arguments
#' @return An object of class `stanfit`
lassoreg_stan <- function(X, y, ...) {
  standata <- list(X = X, y = y, N = length(y), K = ncol(X))
  out <- rstan::sampling(stanmodels$linreg_lasso, data = standata, ...)
  return(out)
}
