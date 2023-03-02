#' Assess confounding effects by Change-in-Estimate approach
#'
#' @param data a data frame.
#' @param outcome outcome variable name.
#' @param time time variable name, for Cox regression.
#' @param exposure exposure variable name.
#' @param covariates covariate names, a vector or a list.
#' @param positive in which positive of outcome variable to make the comparison.
#' By default, positive is automatically defined. If outcome is a factor variable,
#' then positive is defined as the highest level. If outcome is a numerical
#' variable, then positive is defined as the largest value.
#' @param ... further arguments.
#'
#' @return a data frame.
#' @export
CIE <- function(data, outcome = NULL, time = NULL, exposure = NULL, covariates = NULL,
                positive = "auto",
                model = c("auto", "linear", "logit", "cox", "poson", "logbinom"), ...){

  outcome    <- srmisc::select_variable(data, outcome)
  time       <- srmisc::select_variable(data, time)
  exposure   <- srmisc::select_variable(data, exposure)
  covariates <- srmisc::select_variable(data, covariates)
  covariates <- setdiff(covariates, outcome)
  covariates <- setdiff(covariates, exposure)
  covariates <- setdiff(covariates, time)

  if(length(unique(data[[outcome]])) == 2L){
    data <- positive_event(data, outcome, positive = positive)
  }

  model <- auto_model(data, outcome = outcome, time = time, model = model)

}
