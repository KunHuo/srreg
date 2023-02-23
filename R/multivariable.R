#' Multivariable analysis
#'
#' @inheritParams uniariable
#'
#' @return a data frame.
#' @export
multivariable <- function(data,
                          outcome = NULL,
                          time = NULL,
                          indepts = NULL,
                          effect.values =  c("net", "b", "se", "effect", "p"),
                          conf.level = 0.95,
                          conf.brackets = NULL,
                          conf.separator = NULL,
                          digits.pvalue = 3,
                          digits.effect = 2,
                          ref.value = 1,
                          ...) {

  outcome  <- srmisc::select_variable(data, outcome)
  time     <- srmisc::select_variable(data, time)
  indepts <- srmisc::select_variable(data, indepts)

  if(srmisc::is_empty(indepts)){
    indepts <- names(data)
  }

  indepts <- setdiff(indepts, outcome)
  indepts <- setdiff(indepts, time)

  method <- guess_model(data = data, outcome = outcome, time = time)
  frm    <- create_formula(dependent = c(time, outcome), indepts)
  fit    <- srmisc::do_call(method, data = data, formula = frm, ...)

  output <- srmisc::typeset(fit,
                  data = data,
                  outcome = outcome,
                  varnames = indepts,
                  select = effect.values,
                  conf.level = conf.level,
                  conf.brackets = conf.brackets,
                  conf.separator = conf.separator,
                  digits.pvalue = digits.pvalue,
                  digits.effect = digits.effect,
                  ref.value = ref.value)

  title <- switch(method,
                  linear  = "Table: Multivariable multiple linear regression model",
                  logit   = "Table: Multivariable binary logistc regression model",
                  cox     = "Table: Multivariable Cox proportional hazards regression model",
                  default = "Table: Multivariable analysis")

  notes <- switch(method,
                  linear  = "Abbreviation: CI, confidence interval.",
                  logit   = "Abbreviation: OR, adds ratio; CI, confidence interval.",
                  cox     = "Abbreviation: HR, hazard ratio; CI, confidence interval.",
                  default = NULL)

  attr(output, "title") <- title
  attr(output, "note")  <- notes
  class(output) <- c("srreg", "data.frame")
  output
}
