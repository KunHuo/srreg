#' Fitting poisson models
#'
#' @inheritParams logit
#' @export
poson <- function(data, formula, ...){
  stats::glm(formula = formula,
             data = data,
             family = stats::poisson(link = "log"), ...)
}
