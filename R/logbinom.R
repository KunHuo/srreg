#' Fitting log-binomial models
#'
#' @inheritParams logit
#' @export
logbinom <- function(data, formula, ...){
  fit <- stats::glm(formula = formula,
             data = data,
             family = stats::binomial(link = "log"), ...)
  class(fit) <- c("logbinom", class(fit))
  fit
}
