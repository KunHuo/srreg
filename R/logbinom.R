#' Fitting log-binomial models
#'
#' @inheritParams logit
#' @export
#'
#' @seealso [glm[]]
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
#' # Fit a log-binomial model
#' fit <- logbinom(cancer, meta ~ age + sex + race)
#'
#' # Univariable log-binomial regression
#' univariable(fit)
#'
#' # Multivariable log-binomial regression
#' multivariable(fit)
logbinom <- function(data, formula, positive = "auto", ...){

  outcome <- all.vars(formula)[1]
  data <- positive_event(data, outcome, positive = positive)

  fit <- stats::glm(formula = formula,
             data = data,
             family = stats::binomial(link = "log"), ...)
  fit
}



#' Fitting log-binomial models
#'
#' @inheritParams logit2
#' @inherit logit2 return
#'
#' @export
logbinom2 <- function(data, outcome = NULL, exposure = NULL, covariates = NULL, positive = "auto", ...){

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
    if(positive == "auto"){
      positive <- data$positive
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

  logbinom(data, formula = frm, positive = positive, ...)
}
