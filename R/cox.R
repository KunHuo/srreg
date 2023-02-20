#' Fit proportional hazards models
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
#' @param ... Other arguments will be passed to [survival::coxph()].
#'
#' @return an object of class coxph representing the fit.
#' @export
cox <- function(data, formula, ...){
  survival::coxph(data = data, formula = formula, ...)
}
