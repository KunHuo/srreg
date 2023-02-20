#' Fitting logistic models
#'
#' @param data a data frame, list or environment (or object coercible by
#' as.data.frame to a data frame) containing the variables in the model.
#' @param formula an object of class "formula" (or one that can be coerced to
#' that class): a symbolic description of the model to be fitted.
#' @param ... additional arguments to be passed to [glm()] function.
#'
#' @return an object of class inheriting from "glm" which inherits from the
#' class "lm".
#' @export
logit <- function(data, formula, ...){
  stats::glm(formula = formula,
             data = data,
             family = stats::binomial(link = "logit"), ...)
}
