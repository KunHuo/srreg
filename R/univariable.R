#' Univariable analysis
#'
#' @param data a data frame.
#' @param outcome outcome variable name.
#' @param time time variable name, for Cox regression.
#' @param indepts independent variable names.
#' @param model regression models.
#' @param effect.values Effect value, 'ne' for No. of event, 'nt' for No. of
#' total, 'net' for No. of event and total, 'nne' for No. of non-event, 'wald'
#' for Wald Chi-square value, 'B' for coefficients, 'p' for P value, 'se' for
#' standard error, 'or' for odds ratio and its 95% CI, ignore case.
#' @param conf.level The confidence level to use for the confidence interval if
#' conf.int = TRUE. Must be strictly greater than 0 and less than 1. Defaults to
#' 0.95, which corresponds to a 95 percent confidence interval.
#' @param conf.brackets brackets of CI format.
#' @param conf.separator separate of CI format.
#' @param digits.pvalue digits for P value, default 3.
#' @param digits.effect digits for effect value (e.g., OR, HR, or RR), default 2.
#' @param ref.value reference value.
#' @param ... arguments passed to method from [lm()], [glm()] or [survival::coxph()].
#'
#' @return a data frame.
#' @export
univariable <- function(data,
                       outcome = NULL,
                       time = NULL,
                       indepts = NULL,
                       model = c("auto", "linear", "logit", "cox", "poisson", "logbinom", "multinom"),
                       effect.values =  c("net", "b", "se", "effect", "p"),
                       conf.level = 0.95,
                       conf.brackets = NULL,
                       conf.separator = NULL,
                       digits.pvalue = 3,
                       digits.effect = 2,
                       ref.value = 1,
                       ...){

  outcome  <- srmisc::select_variable(data, outcome)
  time     <- srmisc::select_variable(data, time)
  indepts <- srmisc::select_variable(data, indepts)

  if(srmisc::is_empty(indepts)){
    indepts <- names(data)
  }

  indepts <- setdiff(indepts, outcome)
  indepts <- setdiff(indepts, time)

  # model <- match.arg(model)
  model <- auto_model(data = data, outcome = outcome, time = time, model = model)

  output <- lapply(indepts, \(x){
    frm <- create_formula(dependent = c(time, outcome), indepts)
    fit <- srmisc::do_call(model, data = data, formula = frm, ...)
    srmisc::typeset(fit,
                    data = data,
                    outcome = outcome,
                    varnames = x,
                    filter = x,
                    select = effect.values,
                    conf.level = conf.level,
                    conf.brackets = conf.brackets,
                    conf.separator = conf.separator,
                    digits.pvalue = digits.pvalue,
                    digits.effect = digits.effect,
                    ref.value = ref.value)

  })
  output <- do.call(rbind, output)

  title <- switch(model,
                  linear  = "Table: Univariable multiple linear regression model",
                  logit   = "Table: Univariable binary logistc regression model",
                  cox     = "Table: Univariable Cox proportional hazards regression model",
                  "Table: Univariable analysis")

  notes <- switch(model,
                  linear  = "Abbreviation: CI, confidence interval.",
                  logit   = "Abbreviation: OR, adds ratio; CI, confidence interval.",
                  cox     = "Abbreviation: HR, hazard ratio; CI, confidence interval.",
                  default = NULL)

  attr(output, "title") <- title
  attr(output, "note")  <- notes
  class(output) <- c("srreg", "data.frame")
  output
}
