#' Fitting Cox models
#'
#' @description
#' Fits a Cox proportional hazards regression model. Time dependent variables,
#' time dependent strata, multiple events per subject, and other extensions are
#' incorporated using the counting process formulation of Andersen and Gill.
#'
#' @param data a data.frame in which to interpret the variables named in the
#' formula, or in the subset and the weights argument.
#' @param formula a formula object, with the response on the left of a ~ operator,
#' and the terms on the right. The response must be a survival object as returned
#' by the Surv function. For a multi-state model the formula may be a list of formulas.
#' @param positive in which positive of outcome variable to make the comparison.
#' By default, positive is automatically defined. If outcome is a factor variable,
#' then positive is defined as the highest level. If outcome is a numerical
#' variable, then positive is defined as the largest value.
#' @param ... Other arguments will be passed to [survival::coxph()].
#'
#' @return an object of class coxph representing the fit.
#' @export
#'
#' @seealso [survival::coxph()]
#'
#' @examples
#' library(srmisc)
#'
#' # set labels
#' cancer <- codes2labels(cancer, cancer.codes)
#'
#' # Fit a Cox proportional hazards regression
#' fit <- cox(cancer, Surv(time, status) ~ .)
#'
#' # Univariable Cox proportional hazards regression
#' univariable(fit)
#'
#' # Multivariable Cox proportional hazards regression
#' multivariable(fit)
cox <- function(data, formula, positive = "auto", ...){
  outcome <- all.vars(formula)[2]
  data <- positive_event(data, outcome, positive = positive)

  fit <- survival::coxph(data = data, formula = formula, ...)
  fit$data <- data
  fit
}
