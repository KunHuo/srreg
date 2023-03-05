#' Association analysis
#'
#' @description Calculate the association between the two variables by
#' regression models (e.g. linear regression, logistic regression, Cox regression).
#'
#' @param data a data frame.
#' @param outcome outcome variable name.
#' @param time time variable name, for Cox regression.
#' @param exposure exposure variable name.
#' @param covariates covariate names, a vector or a list.
#' @param positive in which positive of outcome variable to make the comparison.
#' By default, positive is automatically defined. If outcome is a factor variable,
#' then positive is defined as the highest level. If outcome is a numerical
#' variable, then positive is defined as the largest value.
#' @param model model regression.
#' @param args arguments passed to method from [lm()], [glm()] or [survival::coxph()].
#' @param p.trend Tests for linear trend when exposure is numeric variable,
#' and set n.quantile.
#' @param n.quantile indicates how many quantiles to convert when exposure is
#' numeric variable.
#' @param quantile.right logical, indicating if the intervals should be closed
#' on the right (and open on the left) or vice versa.
#' @param quantile.labels labels for the levels of the resulting category.
#' By default, labels are constructed using "(a,b]" interval notation. If labels
#' = FALSE, simple integer codes are returned instead of a factor.
#' @param conf.level The confidence level to use for the confidence interval if
#' conf.int = TRUE. Must be strictly greater than 0 and less than 1. Defaults to
#' 0.95, which corresponds to a 95 percent confidence interval.
#' @param conf.brackets brackets of CI format.
#' @param conf.separator separate of CI format.
#' @param digits.pvalue digits for P value, default 3.
#' @param digits.effect digits for effect value (e.g., OR, HR, or RR), default 2.
#' @param ref.value reference value.
#' @param ... further arguments.
#'
#' @return a data.frame.
#' @export
#'
#' @examples
#' library(srmisc)
#'
#' cancer$sex  <- factor(cancer$sex)
#' cancer$race <- factor(cancer$race)
#' cancer$meta <- factor(cancer$meta)
#'
#' # Association Between Metastasis and Tumor size by Linear Regression Model
#' # Model 1 adjusted for age, sex.
#' # Model 2 adjusted for age, sex, and race.
#' associate(data = cancer,
#'           outcome = "size",
#'           exposure = "meta",
#'           covariates = list(c("sex", "age"), c("sex", "age", "race")))
#'
#' # Association Between Race and Metastasis by Logistic Regression Model
#' # Model 1 adjusted for nothing.
#' # Model 2 adjusted for age, sex, and size.
#' associate(data = cancer,
#'           outcome = "meta",
#'           exposure = "race",
#'           covariates = list("Adjusted 1" = NULL,
#'                             "Adjusted 2" = c("sex", "age", "size")))
#'
#' # Association Between Race and Survival status by Cox Proportional Hazards Regression Model
#' # Model 1 adjusted for nothing.
#' # Model 2 adjusted for age, sex, and size.
#' associate(data = cancer,
#'           outcome = "status",
#'           time = "time",
#'           exposure = "race",
#'           covariates = list("Adjusted 1" = NULL,
#'                             "Adjusted 2" = c("sex", "age", "size")))
#'
#' # Tests for linear trend were done by modeling the median value of each quantile
#' # to test ordered relations across quantiles of size.
#' associate(data = cancer,
#'           outcome = "status",
#'           time = "time",
#'           exposure = "size",
#'           covariates = list("Adjusted 1" = NULL,
#'                             "Adjusted 2" = c("sex", "age", "race")),
#'           n.quantile = 4)
associate <- function(data,
                      outcome = NULL,
                      time = NULL,
                      exposure = NULL,
                      covariates = NULL,
                      positive = "auto",
                      model = c("auto", "linear", "logit", "cox", "poson", "logbinom", "multinom"),
                      args = list(),
                      p.trend = TRUE,
                      n.quantile = NULL,
                      quantile.right = TRUE,
                      quantile.labels = NULL,
                      conf.level = 0.95,
                      conf.brackets = NULL,
                      conf.separator = NULL,
                      digits.pvalue = 3,
                      digits.effect = 2,
                      ref.value = "Reference",
                      ...){

  if("taskreg" %in% class(data)){
    if(is.null(outcome)){
      outcome <- data$outcome
    }
    if(is.null(time)){
      time <- data$time
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

    model <- match.arg(model)
    if(model == "auto"){
      model <- data$model
    }
    data <- data$data
  }else{
    outcome    <- srmisc::select_variable(data, outcome)
    time       <- srmisc::select_variable(data, time)
    exposure   <- srmisc::select_variable(data, exposure)
  }

  if(srmisc::is_empty(covariates)){
    covariates <- list(NULL)
  }else{
    if(is.list(covariates)){
      covariates <- lapply(covariates, \(covar){
        covar <- srmisc::select_variable(data, covar)
        covar <- setdiff(covar, outcome)
        covar <- setdiff(covar, time)
        covar <- setdiff(covar, exposure)
        covar
      })
    }else{
      covariates <- srmisc::select_variable(data, covariates)
      covariates <- setdiff(covariates, outcome)
      covariates <- setdiff(covariates, time)
      covariates <- setdiff(covariates, exposure)
      covariates <- list(covariates)
    }
  }

  model <- auto_model(data, outcome = outcome, time = time, model = model)

  # If time is specified, a Cox regression model is fitted;
  # otherwise a model is fitted according to the dependent variable:
  # a logistic regression model for dichotomous variables and
  # a multiple linear regression model for continuous variables.
  model_coef <- function(data, x, covar){
    frm <- create_formula(dependent = c(time, outcome), independents = c(x, covar))
    if(model == "linear"){
      fit <- srmisc::do_call(model, data = data, formula = frm, args)
    }else{
      fit <- srmisc::do_call(model, data = data, formula = frm, positive = positive, args)
    }
    srmisc::typeset(fit,
                    data = data,
                    outcome = outcome,
                    varnames = x,
                    filter = x,
                    conf.level = conf.level,
                    conf.brackets = conf.brackets,
                    conf.separator = conf.separator,
                    digits.pvalue = digits.pvalue,
                    digits.effect = digits.effect,
                    ref.value = ref.value,
                    ...)
  }

  # If the exposed variable is continuous, the exposed variable is grouped
  # according to the quantile specified by n.quantile.
  exec <- function(covar){
    if(!is.null(n.quantile) & is.numeric(data[[exposure]]) ){
      data <- srmisc::cut_quantile(data,
                                   varname = exposure,
                                   n = n.quantile,
                                   right = quantile.right,
                                   labels = quantile.labels)
      # Model for quantiles.
      res1 <- model_coef(data, x = paste0("gq_", exposure), covar)
      if(p.trend){
        # Model for trend.
        res2 <- model_coef(data, x = paste0("mq_", exposure), covar)
        res2 <- res2[, c(1, ncol(res2)), drop = FALSE]
        res1[1, 1] <- srmisc::get_var_label(data, exposure, default = ".name")
        res2[1, 1] <- srmisc::get_var_label(data, exposure, default = ".name")

        if(length(covariates) == 1L){
          names(res2)[2] <- "P for trend a"
        }else{
          names(res2)[2] <- sprintf("P for trend %s", letters[length(covariates) + 1])
        }

        srmisc::merge_left(res1, res2, by = names(res1)[1])
      }else{
        res1[1, 1] <- srmisc::get_var_label(data, exposure, default = ".name")
        res1
      }
    }else{
      model_coef(data, x = exposure, covar)
    }
  }

  # Cyclic execution of regression models
  results <- lapply(covariates, exec)

  # Output
  output <- results[[1]]

  if(length(results) > 1L){
    # Model names
    if(srmisc::is_empty(names(results))){
      MNAMES <-sprintf("Model %d %s", 1:length(results), letters[1:length(results)])
    }else{
      MNAMES <- names(results)
      MNAMES <-sprintf("%s %s", MNAMES, letters[1:length(results)])
    }
    names(output) <- c(names(output)[1:2], paste(MNAMES[1], names(output)[-c(1, 2)], sep = "__"))

    # Merge model results by variable
    for(i in 2:length(results)){
      output <- srmisc::merge_table(output, results[[i]][, -2, drop = FALSE], name.y = MNAMES[i])
    }
  }

  label.exposure <- srmisc::get_var_label(data, exposure, default = ".name")
  label.outcome  <- srmisc::get_var_label(data, outcome,  default = ".name")

  # Output title
  title <- switch(model,
                  linear  = "multiple linear regression",
                  logit   = "binary logistc regression",
                  poson   = "modified Poissson regression",
                  logbinom = "log-binomial regression",
                  cox     = "Cox proportional hazards regression",
                  default = "")
  title <- sprintf("Table: Association between %s and %s using %s",
                   label.exposure,
                   label.outcome,
                   title)

  abbr <- switch(model,
                linear  = "Abbreviation: CI, confidence interval.",
                logit   = "Abbreviation: OR, odds ratio; CI, confidence interval.",
                poson   = "Abbreviation: RR, risk ratio; CI, confidence interval.",
                logbinom = "Abbreviation: RR, risk ratio; CI, confidence interval.",
                cox     = "Abbreviation: HR, hazard ratio; CI, confidence interval.",
                "Abbreviation: CI, confidence interval.")

  # Output notes
  notes <- sapply(covariates, function(x){
    if(srmisc::is_empty(x)){
      "Adjusted for nonthing."
    }else{
      x <- srmisc::get_var_label(data, x, default = ".name")
      sprintf("Adjusted for %s.", paste(x, collapse = ", "))
    }
  })
  if(length(notes) == 1L){
    notes <- notes[[1]]
  }else{
    notes <- sprintf("%s %s", letters[1:length(notes)], notes)
    notes <- paste(notes, collapse = "\n")
  }

  # Note of p for trend.
  if(!is.null(n.quantile) & p.trend){
    if(n.quantile == 3L){
      note.trend <- "Tests for linear trend were done by modeling the median value of each tertile to test ordered relations across tertiles of %s."
    }else if(n.quantile == 4L){
      note.trend <- "Tests for linear trend were done by modeling the median value of each quantile to test ordered relations across quantiles of %s."
    }else{
      note.trend <- "Tests for linear trend were done by modeling the median value of each group to test ordered relations across quantiles of %s."
    }
    note.trend <- sprintf(note.trend, srmisc::get_var_label(data, exposure, default = ".name"))
    if(length(results) == 1L){
      notes <- paste(notes, sprintf("%s %s.", "a", note.trend), sep = "\n")
    }else{
      notes <- paste(notes, sprintf("%s %s.", letters[length(results) + 1], note.trend), sep = "\n")
    }
  }

  notes <- paste(abbr, notes, sep = "\n")

  attr(output, "title") <- title
  attr(output, "note")  <- notes
  class(output) <- c("srreg", "data.frame")
  output
}
