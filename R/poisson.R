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
