#' Fitting log-binomial models
#'
#' @inheritParams logit
#' @export
logbinom <- function(data, formula, positive = "auto", ...){

  outcome <- all.vars(formula)[1]
  data <- positive_event(data, outcome, positive = positive)

  fit <- stats::glm(formula = formula,
             data = data,
             family = stats::binomial(link = "log"), ...)
  fit
}
