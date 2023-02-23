#' Fitting poisson models
#'
#' @inheritParams logit
#' @export
poisson <- function(data, formula, ...){
  stats::glm(formula = formula,
             data = data,
             family = stats::poisson(link = "log"), ...)
}
