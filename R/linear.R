#' Fitting linear models
#'
#' @description
#' linear is used to fit linear models, including multivariate ones. It can be
#' used to carry out regression, single stratum analysis of variance and analysis
#' of covariance (although aov may provide a more convenient interface for these).
#'
#' @param data a data frame, list or environment (or object coercible by as.data.
#' frame to a data frame) containing the variables in the model.
#' @param formula an object of class "formula" (or one that can be coerced to
#' that class): a symbolic description of the model to be fitted.
#' @param ... additional arguments to be passed to [lm()] function.
#'
#' @return an object of class "lm" or for multivariate ('multiple') responses
#' of class c("mlm", "lm").
#' @export
linear <- function(data, formula, ...){
  fit <- stats::lm(data = data, formula = formula, ...)
  class(fit) <- c("linear", class(fit))
  fit
}
