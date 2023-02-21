associate <- function(data,
                      outcome = NULL,
                      time = NULL,
                      exposure = NULL,
                      covariates = NULL,
                      args = list(),
                      p.trend = TRUE,
                      n.quantile = NULL,
                      quantile.right = TRUE,
                      quantile.labels = NULL,
                      ...){

  outcome    <- srmisc::select_variable(data, outcome)
  time       <- srmisc::select_variable(data, time)
  exposure   <- srmisc::select_variable(data, exposure)

  if(srmisc::is_empty(covariates)){
    covariates <- list(NULL)
  }else{
    if(is.list(covariates)){
      covariates <- lapply(covariates, \(covar){
        srmisc::select_variable(data, covar)
      })
    }else{
      covariates <- srmisc::select_variable(data, covariates)
      covariates <- list(covariates)
    }
  }

  # If time is specified, a Cox regression model is fitted;
  # otherwise a model is fitted according to the dependent variable:
  # a logistic regression model for dichotomous variables and
  # a multiple linear regression model for continuous variables.
  model <- function(data, x, covar){
    indepts <- c(x, covar)
    if (is.null(time)) {
      frm <- create_formula(dependent = outcome, independents = indepts)
      if(length(unique(data[[outcome]])) == 2L){
        fit <- srmisc::do_call(logit, data = data, formula = frm, args)
      }else{
        if(is.numeric(data[[outcome]])){
          fit <- srmisc::do_call(linear, data = data, formula = frm, args)
        }
      }
    }else{
      frm <- create_formula(dependent = c(time, outcome), independents = indepts)
      fit <- srmisc::do_call(cox, data = data, formula = frm, args)
    }
    srmisc::typeset(fit, data = data, outcome = outcome, varnames = x, filter = x,  ...)
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
      res1 <- model(data, x = paste0("gq_", exposure), covar)
      if(p.trend){
        # Model for trend.
        res2 <- model(data, x = paste0("mq_", exposure), covar)
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
      model(data, x = exposure, covar)
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

    # Merge model results bt variable
    for(i in 2:length(results)){
      output <- srmisc::merge_table(output, results[[i]][, -2, drop = FALSE], name.y = MNAMES[i])
    }
  }

  # Output title
  if(is.null(time)){
    if(length(unique(data[[outcome]])) == 2L){
      title <- "logistic"
    }else{
      title <- "linear"
    }
  }else{
    title <- "Cox"
  }

  # Output notes
  notes <- sapply(covariates, function(x){
    if(srmisc::is_empty(x)){
      "Adjusted for nonthing."
    }else{
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
      note.trend <- "Tests for linear trend were done by modeling the median value of each tertile to test ordered relations across tertiles of %s"
    }else if(n.quantile == 4L){
      note.trend <- "Tests for linear trend were done by modeling the median value of each quantile to test ordered relations across quantiles of %s"
    }else{
      note.trend <- "Tests for linear trend were done by modeling the median value of each group to test ordered relations across quantiles of %s"
    }
    note.trend <- sprintf(note.trend, srmisc::get_var_label(data, exposure, default = ".name"))
    if(length(results) == 1L){
      notes <- c(notes, sprintf("\n%s %s.", "a", note.trend))
    }else{
      notes <- c(notes, sprintf("\n%s %s.", letters[length(results) + 1], note.trend))
    }
  }

  attr(output, "title") <- title
  attr(output, "note")  <- notes
  output
}
