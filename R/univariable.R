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
                        model = c("auto", "linear", "logit", "cox", "poson", "logbinom", "multinom"),
                        effect.values =  c("b", "se", "effect", "p"),
                        conf.level = 0.95,
                        conf.brackets = NULL,
                        conf.separator = NULL,
                        digits.pvalue = 3,
                        digits.effect = 2,
                        ref.value = "Reference",
                        ...){
  UseMethod("univariable")
}


#' Univariable analysis for data.frame
#'
#' @inheritParams univariable
#' @inherit univariable return
#' @keywords internal
#' @export
univariable.data.frame <- function(data,
                                   outcome = NULL,
                                   time = NULL,
                                   indepts = NULL,
                                   model = c("auto", "linear", "logit", "cox", "poson", "logbinom", "multinom"),
                                   effect.values =  c("b", "se", "effect", "p"),
                                   conf.level = 0.95,
                                   conf.brackets = NULL,
                                   conf.separator = NULL,
                                   digits.pvalue = 3,
                                   digits.effect = 2,
                                   ref.value = "Reference",
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
                  linear  = "Univariable linear regression",
                  logit   = "Univariable binary logistc regression",
                  cox     = "Univariable Cox proportional hazards regression",
                  poson   = "Univariable modified Poissson regression",
                  logbinom = "Univariable log-binomial regression",
                  "Univariable analysis")

  notes <- switch(model,
                  linear  = "Abbreviation: CI, confidence interval.",
                  logit   = "Abbreviation: OR, odds ratio; CI, confidence interval.",
                  poson   = "Abbreviation: RR, risk ratio; CI, confidence interval.",
                  logbinom = "Abbreviation: RR, risk ratio; CI, confidence interval.",
                  cox     = "Abbreviation: HR, hazard ratio; CI, confidence interval.",
                  "Abbreviation: CI, confidence interval.")

  attr(output, "title") <- title
  attr(output, "note")  <- notes
  class(output) <- c("srreg", "data.frame")
  output
}


#' Univariable analysis for lm
#'
#' @inheritParams univariable
#' @inherit univariable return
#' @keywords internal
#' @export
univariable.lm <- function(data,
                               outcome = NULL,
                               time = NULL,
                               indepts = NULL,
                               model = c("linear"),
                               effect.values =  c("b", "se", "effect", "p"),
                               conf.level = 0.95,
                               conf.brackets = NULL,
                               conf.separator = NULL,
                               digits.pvalue = 3,
                               digits.effect = 2,
                               ref.value = "Reference",
                               ...){
   d <- data$model
   outcome <- all.vars(data$terms)[1]
   indepts <- all.vars(attr(data$model, "terms"))[-1]

   univariable.data.frame(data = d,
                          outcome = outcome,
                          indepts = indepts,
                          model = model,
                          effect.values = effect.values,
                          conf.level = conf.level,
                          conf.brackets = conf.brackets,
                          conf.separator = conf.separator,
                          digits.pvalue = digits.pvalue,
                          digits.effect = digits.effect,
                          ref.value = ref.value,
                          ...)
}



#' Univariable analysis for glm
#'
#' @inheritParams univariable
#' @inherit univariable return
#' @keywords internal
#' @export
univariable.glm <- function(data,
                           outcome = NULL,
                           time = NULL,
                           indepts = NULL,
                           model = c("linear", "logit", "poson", "logbinom"),
                           effect.values =  c("b", "se", "effect", "p"),
                           conf.level = 0.95,
                           conf.brackets = NULL,
                           conf.separator = NULL,
                           digits.pvalue = 3,
                           digits.effect = 2,
                           ref.value = "Reference",
                           ...){
  d <- data$data
  outcome <- all.vars(data$formula)[1]
  indepts <- all.vars(data$formula)[-1]

  if(data$family$family == "gaussian"){
    model <- "linear"
  }else if(data$family$family == "binomial"){
    if(data$family$link == "logit"){
      model <- "logit"
    }else if(data$family$link == "log"){
      model <- "logbinom"
    }else{
      model <- "logit"
    }
  }else if(data$family$family == "poisson"){
    model <- "poson"
  }

  univariable.data.frame(data = d,
                         outcome = outcome,
                         indepts = indepts,
                         model = model,
                         effect.values = effect.values,
                         conf.level = conf.level,
                         conf.brackets = conf.brackets,
                         conf.separator = conf.separator,
                         digits.pvalue = digits.pvalue,
                         digits.effect = digits.effect,
                         ref.value = ref.value,
                         ...)
}


#' Univariable analysis for coxph
#'
#' @inheritParams univariable
#' @inherit univariable return
#' @keywords internal
#' @export
univariable.coxph <- function(data,
                              outcome = NULL,
                              time = NULL,
                              indepts = NULL,
                              model = c("cox"),
                              effect.values =  c("b", "se", "effect", "p"),
                              conf.level = 0.95,
                              conf.brackets = NULL,
                              conf.separator = NULL,
                              digits.pvalue = 3,
                              digits.effect = 2,
                              ref.value = "Reference",
                              ...){

  d <- data$data

  if(is.null(d)){
    stop("Use 'cox' function fit a Cox proportional hazards regression instead of 'coxph'.", call. = FALSE)
  }

  time <- all.vars(data$formula)[1]
  outcome <- all.vars(data$formula)[2]
  indepts <-  all.vars(data$formula)[-c(1:2)]

  univariable.data.frame(data = d,
                         outcome = outcome,
                         time = time,
                         indepts = indepts,
                         model = model,
                         effect.values = effect.values,
                         conf.level = conf.level,
                         conf.brackets = conf.brackets,
                         conf.separator = conf.separator,
                         digits.pvalue = digits.pvalue,
                         digits.effect = digits.effect,
                         ref.value = ref.value,
                         ...)

}

