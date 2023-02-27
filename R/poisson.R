#' Fitting poisson models
#'
#' @inheritParams logit
#' @export
poson <- function(data, formula, positive = "auto", ...){
  outcome <- all.vars(formula)[1]
  if(length(unique(data[[outcome]])) == 2L){
    data <- positive_event(data, outcome, positive = positive)
  }

  fit <- stats::glm(formula = formula,
             data = data,
             family = stats::poisson(link = "log"), ...)
  fit
}
