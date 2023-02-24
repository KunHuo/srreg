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
                          effect.values =  c("b", "se", "effect", "p"),
                          conf.level = 0.95,
                          conf.brackets = NULL,
                          conf.separator = NULL,
                          digits.pvalue = 3,
                          digits.effect = 2,
                          ref.value = "Referrence",
                          ...){
  UseMethod("multivariable")
}

#' Multivariable analysis for data.frame
#'
#' @inheritParams multivariable
#' @inherit multivariable return
#' @keywords internal
#' @export
multivariable.data.frame <- function(data,
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

  attr(output, "title") <- multivariable_title(model)
  attr(output, "note")  <- multivariable_note(model)
  class(output) <- c("srreg", "data.frame")
  output
}


#' Multivariable analysis for lm
#'
#' @inheritParams multivariable
#' @inherit multivariable return
#' @keywords internal
#' @export
multivariable.lm <- function(data,
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
                                 ref.value = "Referrence",
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

  attr(output, "title") <- multivariable_title(model)
  attr(output, "note")  <- multivariable_note(model)
  class(output) <- c("srreg", "data.frame")
  output
}


#' Multivariable analysis for glm
#'
#' @inheritParams multivariable
#' @inherit multivariable return
#' @keywords internal
#' @export
multivariable.glm <- function(data,
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
                             ref.value = "Referrence",
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

  attr(output, "title") <- multivariable_title(model)
  attr(output, "note")  <- multivariable_note(model)
  class(output) <- c("srreg", "data.frame")
  output
}


#' Multivariable analysis for coxph
#'
#' @inheritParams multivariable
#' @inherit multivariable return
#' @keywords internal
#' @export
multivariable.coxph <- function(data,
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
                                ref.value = "Referrence",
                                ...) {

  d <- data$data

  if(is.null(d)){
    stop("Use 'cox' function fit a Cox proportional hazards regression instead of 'coxph'.", call. = FALSE)
  }

  output <- srmisc::typeset(x = data,
                            data = d,
                            select = effect.values,
                            conf.level = conf.level,
                            conf.brackets = conf.brackets,
                            conf.separator = conf.separator,
                            digits.pvalue = digits.pvalue,
                            digits.effect = digits.effect,
                            ref.value = ref.value,
                            ...)

  attr(output, "title") <- multivariable_title(model)
  attr(output, "note")  <- multivariable_note(model)
  class(output) <- c("srreg", "data.frame")
  output
}


multivariable_title <- function(model){
  switch(model,
         linear  = "Multivariable linear regression",
         logit   = "Multivariable binary logistc regression",
         poson   = "Multivariable modified Poissson regression",
         logbinom = "Multivariable log-binomial regression",
         cox      = "Multivariable Cox proportional hazards regression",
         "Multivariable analysis")
}


multivariable_note <- function(model){
 switch(model,
        linear  = "Abbreviation: CI, confidence interval.",
        logit   = "Abbreviation: OR, odds ratio; CI, confidence interval.",
        poson   = "Abbreviation: RR, risk ratio; CI, confidence interval.",
        logbinom = "Abbreviation: RR, risk ratio; CI, confidence interval.",
        cox     = "Abbreviation: HR, hazard ratio; CI, confidence interval.",
        "Abbreviation: CI, confidence interval.")
}

