#' Fitting linear models
#'
#' @description
#' linear is used to fit linear models, including multivariate ones. It can be
#' used to carry out regression, single stratum analysis of variance and analysis
#' of covariance (although aov may provide a more convenient interface for these).
#'
#' @param data a data frame, list or environment (or object coercible by as.data.
#' frame to a data frame) containing the variables in the model.
#' @param formula an object of class "formula" (or one that can be coerced to
#' that class): a symbolic description of the model to be fitted.
#' @param ... additional arguments to be passed to [lm()] function.
#'
#' @return an object of class "lm" or for multivariate ('multiple') responses
#' of class c("mlm", "lm").
#' @export
#'
#' @seealso [lm()]
#'
#' @examples
#' # Fit a linear model
#' fit <- linear(iris, Sepal.Length ~ .)
#'
#' # Coefficients of univariable linear regression
#' univariable(fit)
#'
#' # Coefficients of multivariable linear regression
#' multivariable(fit)
linear <- function(data, formula, ...){
  stats::lm(data = data, formula = formula, ...)

}


#' Fitting linear models
#'
#' @param data a data frame, list or environment (or object coercible by as.data.
#' frame to a data frame) containing the variables in the model.
#' @param outcome outcome variable name.
#' @param exposure exposure variable name.
#' @param covariates covariate names, a vector or a list.
#' @param ... additional arguments to be passed to [lm()] function.
#'
#' @return an object of class "lm" or for multivariate ('multiple') responses
#' of class c("mlm", "lm").
#'
#' @export
linear2 <- function(data, outcome = NULL, exposure = NULL, covariates = NULL, ...){
  if("taskreg" %in% class(data)){
    if(is.null(outcome)){
      outcome <- data$outcome
    }
    if(is.null(exposure)){
      exposure <- data$exposure
    }
    if(is.null(covariates)){
      covariates <- data$covariates
    }
    covariates <- setdiff(covariates, outcome)
    covariates <- setdiff(covariates, exposure)
    data <- data$data
  }else{
    outcome    <- srmisc::select_variable(data, outcome)
    exposure   <- srmisc::select_variable(data, exposure)
    covariates <- srmisc::select_variable(data, covariates)
    covariates <- setdiff(covariates, outcome)
    covariates <- setdiff(covariates, exposure)
  }

  frm <- create_formula(outcome, independents = c(exposure, covariates))
  linear(data, formula = frm, ...)
}


