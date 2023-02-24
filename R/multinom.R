#' Fitting multinomial logistic models
#'
#' @inheritParams logit
#' @seealso [nnet::multinom()]
#' @export
multinom <- function(data, formula, ...){
  fit <- nnet::multinom(formula = formula, data = data, ...)
  fit
}
