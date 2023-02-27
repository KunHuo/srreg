#' Fitting logistic models
#'
#' @param data a data frame, list or environment (or object coercible by
#' as.data.frame to a data frame) containing the variables in the model.
#' @param formula an object of class "formula" (or one that can be coerced to
#' that class): a symbolic description of the model to be fitted.
#' @param positive in which positive of outcome variable to make the comparison.
#' By default, positive is automatically defined. If outcome is a factor variable,
#' then positive is defined as the highest level. If outcome is a numerical
#' variable, then positive is defined as the largest value.
#' @param ... additional arguments to be passed to [glm()] function.
#'
#' @return an object of class inheriting from "glm" which inherits from the
#' class "lm".
#' @export
#'
#' @seealso [glm()]
#'
#' @examples
#' library(srmisc)
#'
#' data("cancer")
#'
#' cancer$sex  <- factor(cancer$sex)
#' cancer$race <- factor(cancer$race)
#' cancer$meta <- factor(cancer$meta)
#'
#' # Fit a binary logistic model
#' fit <- logit(cancer, meta ~ age + sex + race + size)
#'
#' # Univariable binary logistc regression
#' univariable(fit)
#'
#' # Multivariable binary logistc regression
#' multivariable(fit)
logit <- function(data, formula, positive = "auto", ...){

  outcome <- all.vars(formula)[1]
  data <- positive_event(data, outcome, positive = positive)

  fit <- stats::glm(formula = formula,
             data = data,
             family = stats::binomial(link = "logit"), ...)
  class(fit) <- c("logit", class(fit))
  fit
}
