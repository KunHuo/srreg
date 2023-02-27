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
