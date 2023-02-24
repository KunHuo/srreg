#' Fitting poisson models
#'
#' @inheritParams logit
#' @export
poson <- function(data, formula, ...){
  fit <- stats::glm(formula = formula,
             data = data,
             family = stats::poisson(link = "log"), ...)
  fit
}
