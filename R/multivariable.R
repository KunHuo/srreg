#' Multivariable analysis
#'
#' @inheritParams univariable
#'
#' @return a data frame.
#' @export
multivariable <- function(data,
                          outcome = NULL,
                          time = NULL,
                          indepts = NULL,
                          model = c("auto", "linear", "logit", "cox", "poson", "logbinom", "multinom"),
                          effect.values =  c("net", "b", "se", "effect", "p"),
                          conf.level = 0.95,
                          conf.brackets = NULL,
                          conf.separator = NULL,
                          digits.pvalue = 3,
                          digits.effect = 2,
                          ref.value = "Referrence",
                          ...){
  UseMethod("multivariable")
}

#' @rdname multivariable
#' @export
multivariable.data.frame <- function(data,
                                     outcome = NULL,
                                     time = NULL,
                                     indepts = NULL,
                                     model = c("auto", "linear", "logit", "cox", "poson", "logbinom", "multinom"),
                                     effect.values =  c("net", "b", "se", "effect", "p"),
                                     conf.level = 0.95,
                                     conf.brackets = NULL,
                                     conf.separator = NULL,
                                     digits.pvalue = 3,
                                     digits.effect = 2,
                                     ref.value = "Referrence",
                                     ...) {

  outcome  <- srmisc::select_variable(data, outcome)
  time     <- srmisc::select_variable(data, time)
  indepts <- srmisc::select_variable(data, indepts)

  if(srmisc::is_empty(indepts)){
    indepts <- names(data)
  }

  indepts <- setdiff(indepts, outcome)
  indepts <- setdiff(indepts, time)

  model <- auto_model(data = data, outcome = outcome, time = time, model = model)
  frm    <- create_formula(dependent = c(time, outcome), indepts)
  fit    <- srmisc::do_call(model, data = data, formula = frm, ...)

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

  title <- switch(model,
                  linear  = "Table: Multivariable multiple linear regression model",
                  logit   = "Table: Multivariable binary logistc regression model",
                  cox     = "Table: Multivariable Cox proportional hazards regression model",
                  default = "Table: Multivariable analysis")

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


#' @rdname multivariable
#' @export
multivariable.linear <- function(data,
                                 outcome = NULL,
                                 time = NULL,
                                 indepts = NULL,
                                 model = c("linear"),
                                 effect.values =  c("n", "b", "se", "effect", "p"),
                                 conf.level = 0.95,
                                 conf.brackets = NULL,
                                 conf.separator = NULL,
                                 digits.pvalue = 3,
                                 digits.effect = 2,
                                 ref.value = 0,
                                ...) {

  output <- srmisc::typeset(data,
                            select = effect.values,
                            conf.level = conf.level,
                            conf.brackets = conf.brackets,
                            conf.separator = conf.separator,
                            digits.pvalue = digits.pvalue,
                            digits.effect = digits.effect,
                            ref.value = ref.value,
                            ...)

  attr(output, "title") <- "Table: Multivariable multiple linear regression model"
  attr(output, "note")  <- "Abbreviation: CI, confidence interval."
  class(output) <- c("srreg", "data.frame")
  output
}
