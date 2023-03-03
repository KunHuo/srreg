#' Task for regression
#'
#' @param data a data frame.
#' @param outcome outcome variable name.
#' @param time time variable name, for Cox regression.
#' @param exposure exposure variable name.
#' @param covariates covariate names, a vector or a list.
#' @param strata stratified varaible names.
#' @param positive in which positive of outcome variable to make the comparison.
#' By default, positive is automatically defined. If outcome is a factor variable,
#' then positive is defined as the highest level. If outcome is a numerical
#' variable, then positive is defined as the largest value.
#' @param model model regression.
#' @param ... unused.
#'
#' @return a list
#' @export
taskreg <- function(data,
                    outcome = NULL,
                    time = NULL,
                    exposure = NULL,
                    covariates = NULL,
                    strata = NULL,
                    positive = "auto",
                    model = c("auto", "linear", "logit", "cox", "poson", "logbinom", "multinom"),
                    ...){

  outcome    <- srmisc::select_variable(data, outcome)
  time       <- srmisc::select_variable(data, time)
  exposure   <- srmisc::select_variable(data, exposure)
  covariates <- srmisc::select_variable(data, covariates)
  strata <- srmisc::select_variable(data, strata)

  covariates <- setdiff(covariates, outcome)
  covariates <- setdiff(covariates, exposure)
  covariates <- setdiff(covariates, time)

  strata <- setdiff(strata, outcome)
  strata <- setdiff(strata, time)
  strata <- setdiff(strata, exposure)

  model <- match.arg(model)
  model <- auto_model(data = data, outcome = outcome, time = time, model = model)

  out <- list(
    data = data,
    outcome = outcome,
    time = time,
    exposure = exposure,
    covariates = covariates,
    strata = strata,
    positive = positive,
    model = model)

  class(out) <- c("taskreg", "list")
  out
}
