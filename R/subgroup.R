#' Subgroup analysis
#'
#' @description Calculate the association between the two variables stratified by
#' different factors using regression models (e.g. linear regression, logistic
#' regression, Cox regression).
#'
#' @inheritParams associate
#' @param strata stratified varaible names.
#'
#' @details
#' P values for interaction were evaluated using interaction terms and likelihood
#' ratio tests.
#'
#' @return a data frame.
#' @export
subgroup <- function(data,
                    outcome = NULL,
                    time = NULL,
                    exposure = NULL,
                    covariates = NULL,
                    strata = NULL,
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

  outcome    <- srmisc::select_variable(data, outcome)
  time       <- srmisc::select_variable(data, time)
  exposure   <- srmisc::select_variable(data, exposure)
  covariates <- srmisc::select_variable(data, covariates)
  strata     <- srmisc::select_variable(data, strata)

  if(srmisc::is_empty(strata)){
    strata <- setdiff(names(data), outcome)
    strata <- setdiff(strata, time)
    strata <- setdiff(strata, exposure)
  }
  strata <- srmisc::select_factor(data, varnames = strata)
  strata <- setdiff(strata, outcome)
  strata <- setdiff(strata, exposure)

  # Functions executed with a unique value of the exposure variable less than or equal to 2.
  exec2 <- function(svar){
    gres <-  srmisc::group_exec(data, group = svar, func = function(d){
      d <-  droplevels.data.frame(d)
      res <- associate(data = d,
                outcome = outcome,
                time = time,
                exposure = exposure,
                covariates = setdiff(covariates, svar),
                positive = positive,
                model = model,
                select = c("effect", "p"),
                conf.level = conf.level,
                conf.brackets = conf.brackets,
                conf.separator = conf.separator,
                digits.pvalue = digits.pvalue,
                digits.effect = digits.effect,
                ref.value = ref.value, ...)
      if(!is.numeric(data[[exposure]])){
        res <- res[-c(1, 2), , drop = FALSE]
      }
      res
    }, warning = FALSE)

    if(!is.null(gres)){
      gres <- gres[, -2, drop = FALSE]
    }
    gres
  }

  # Functions executed with a unique value of the exposure variable great than 2.
  exec3 <- function(svar){
    gres <- srmisc::group_exec(data, group = svar, func = function(d){
      d <-  droplevels.data.frame(d)
      res <- associate(data = d,
                       outcome = outcome,
                       time = time,
                       exposure = exposure,
                       covariates = setdiff(covariates, svar),
                       positive = positive,
                       model = model,
                       select = c("effect"),
                       conf.level = conf.level,
                       conf.brackets = conf.brackets,
                       conf.separator = conf.separator,
                       digits.pvalue = digits.pvalue,
                       digits.effect = digits.effect,
                       ref.value = ref.value,
                       ...)
      res[-1, , drop = FALSE]
    }, warning = FALSE)

    if(!is.null(gres)){
      gres$id <- rep(1:length(unique(data[[svar]])),
                     each = length(unique(data[[exposure]])))
      gres[[2]] <- trimws(gres[[2]])
      gres <- srmisc::reshape_wide(gres,
                                   id = "id",
                                   names.from = 2,
                                   values.from = 3,
                                   include.id = FALSE)
    }
    gres
  }

  exec_numeric <- function(svar){
    gres <- srmisc::group_exec(data, group = svar, func = function(d){
      d <-  droplevels.data.frame(d)
      res <- associate(data = d,
                       outcome = outcome,
                       time = time,
                       exposure = paste0("gq_", exposure),
                       covariates = setdiff(covariates, svar),
                       positive = positive,
                       model = model,
                       select = c("effect"),
                       conf.level = conf.level,
                       conf.brackets = conf.brackets,
                       conf.separator = conf.separator,
                       digits.pvalue = digits.pvalue,
                       digits.effect = digits.effect,
                       ref.value = ref.value,
                       ...)
      res[-1, , drop = FALSE]
    }, warning = FALSE)

    if(!is.null(gres)){
      gres$id <- rep(1:length(unique(gres[[svar]])),
                     each = length(unique(data[[paste0("gq_", exposure)]])))
      gres[[2]] <- trimws(gres[[2]])
      gres <- srmisc::reshape_wide(gres,
                                   id = "id",
                                   names.from = 2,
                                   values.from = 3,
                                   include.id = FALSE)
      if(p.trend){
        trend <- srmisc::group_exec(data, group = svar, func = function(d){
          d <-  droplevels.data.frame(d)
          associate(data = d,
                    outcome = outcome,
                    time = time,
                    exposure = paste0("mq_", exposure),
                    covariates = setdiff(covariates, svar),
                    positive = positive,
                    model = model,
                    select = c("p.value"),
                    conf.level = conf.level,
                    conf.brackets = conf.brackets,
                    conf.separator = conf.separator,
                    digits.pvalue = digits.pvalue,
                    digits.effect = digits.effect,
                    ref.value = ref.value,
                    ...)
        }, warning = FALSE)

        if(!is.null(trend)){
          trend <- trend[, -2, drop = FALSE]
          names(trend)[2] <- "P for trend b"
          gres <- srmisc::merge_left(gres, trend, by = names(gres)[1])
        }
      }
    }
    gres
  }

  # Loop execution function
  if(is.factor(data[[exposure]]) | is.character(data[[exposure]])){
    if(length(unique(data[[exposure]])) < 3L){
      results <- lapply(strata, exec2)
    }else{
      results <- lapply(strata, exec3)
    }
  }else{
    if(!is.null(n.quantile)){
      data <- srmisc::cut_quantile(data,
                                   varname = exposure,
                                   n = n.quantile,
                                   right = quantile.right,
                                   labels = quantile.labels)
      results <- lapply(strata, exec_numeric)
    }else{
      results <- lapply(strata, exec2)
    }
  }

  # Formatting results
  results <- results[!sapply(results, is.null)]

  results <- lapply(results, function(x){
    x[[1]] <- paste(names(x)[1], x[[1]], sep = "")
    names(x)[1] <- "term"

    if(ncol(x) >= 4L){
      exposure.label <- srmisc::get_var_label(data, exposure, default = ".name")
      if(is.null(time)){
        if(length(unique(data[[outcome]])) == 2L){
          exposure.label <- sprintf("%s, OR (%s%% CI)", exposure.label, as.character(conf.level * 100))
        }else{
          exposure.label <- sprintf("%s, \u3b2 (%s%% CI)", exposure.label, as.character(conf.level * 100))
        }
      }else{
        exposure.label <- sprintf("%s, HR (%s%% CI)", exposure.label, as.character(conf.level * 100))
      }

      if("P for trend b" %in% names(x)){
        names(x)[2:(ncol(x) - 1)] <- paste(exposure.label, names(x)[c(-1, -ncol(x))], sep = "__")
      }else{
        names(x)[2:ncol(x)] <- paste(exposure.label, names(x)[-1], sep = "__")
      }
    }
    x
  })
  results <- do.call(rbind, results)

  # Output variable
  output <- srmisc::fmt_reg(data = data, varnames = strata)

  # Describe event
  desc.method <- ifelse(length(unique(data[[outcome]])) == 2L, "n.event.total", "n.total")
  desc   <- srmisc::describe_event(data = data,
                                   event = outcome,
                                   varnames = strata,
                                   method = desc.method)
  desc <- desc[, c("term", desc.method)]
  names(desc)[2] <- ifelse(desc.method == "n.total", "No. of total", "No. of event/total")
  output <- srmisc::merge_left(output, desc, by = "term")


  if(!is.null(n.quantile) & is.numeric(data[[exposure]])){
    LRT.exposure <- paste0("gq_", exposure)
  }else{
    LRT.exposure <- exposure
  }

  LRT.results <- LRT(data = data,
                     outcome = outcome,
                     time = time,
                     exposure = LRT.exposure,
                     covariates = covariates,
                     strata = strata,
                     positive = positive,
                     model = model,
                     args = args,
                     digits.pvalue = digits.pvalue)

  output <- srmisc::merge_left(output, results, by = "term")
  output <- srmisc::merge_left(output, LRT.results, by = "term")
  output <- output[, -c(1:3), drop = FALSE]
  names(output)[1] <- "Subgroup"


  # Output title
  if(is.null(time)){
    if(length(unique(data[[outcome]])) == 2L){
      title <- sprintf("Table: Association of %s with %s stratified by different factors using logistic regression model a",
                       srmisc::get_var_label(data, exposure, default = ".name"),
                       srmisc::get_var_label(data, outcome,  default = ".name"))
      abbr <- "Abbreviation: OR, odds ratio; CI, confidence interval."
    }else{
      title <- sprintf("Table: Association of %s with %s stratified by different factors using linear regression model a",
                       srmisc::get_var_label(data, exposure, default = ".name"),
                       srmisc::get_var_label(data, outcome,  default = ".name"))
      abbr <- "Abbreviation: CI, confidence interval."
    }
  }else{
    title <- sprintf("Table: Association of %s with %s stratified by different factors using Cox proportional hazards regression model a",
                     srmisc::get_var_label(data, exposure, default = ".name"),
                     srmisc::get_var_label(data, outcome,  default = ".name"))
    abbr <- "Abbreviation: HR, hazard ratio; CI, confidence interval."
  }

  if(srmisc::is_empty(covariates)){
    notes <- paste(abbr, "a Adjusted for nothing.", sep = "\n")
  }else{
    notes <- paste(covariates, collapse = ", ")
    notes <- paste("a Adjusted for ", notes, ", but exclude stratified variable.", sep = "")
    notes <- paste(abbr, notes, sep = "\n")
  }

  if("P for trend b" %in% names(output)){
    if(n.quantile == 3L){
      note.trend <- "b Tests for linear trend were done by modeling the median value of each tertile to test ordered relations across tertiles of %s"
    }else if(n.quantile == 4L){
      note.trend <- "b Tests for linear trend were done by modeling the median value of each quantile to test ordered relations across quantiles of %s"
    }else{
      note.trend <- "b Tests for linear trend were done by modeling the median value of each group to test ordered relations across quantiles of %s"
    }
    note.trend <- sprintf(note.trend, srmisc::get_var_label(data, exposure, default = ".name"))
    notes <- paste(notes, note.trend, sep = "\n")
  }

  attr(output, "title") <- title
  attr(output,  "note") <- notes

  class(output) <- c("srreg", "data.frame")
  output
}
