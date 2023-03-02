#' Fitting poisson models
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
#' # Fit a modified Poissson model
#' fit <- poson(cancer, meta ~ age + sex + race + size)
#'
#' # Univariable modified Poissson regression
#' univariable(fit)
#'
#' # Multivariable modified Poissson regression
#' multivariable(fit)
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


#' Fitting poisson models
#'
#' @inheritParams logit2
#' @inherit logit2 return
#'
#' @export
poson2 <- function(data, outcome = NULL, exposure = NULL, covariates = NULL, positive = "auto", ...){

  outcome    <- srmisc::select_variable(data, outcome)
  exposure   <- srmisc::select_variable(data, exposure)
  covariates <- srmisc::select_variable(data, covariates)
  covariates <- setdiff(covariates, outcome)
  covariates <- setdiff(covariates, exposure)

  frm <- create_formula(outcome, independents = c(exposure, covariates))

  poson(data, formula = frm, positive = positive, ...)
}
