associate <- function(data,
                      outcome = NULL,
                      time = NULL,
                      exposure = NULL,
                      covariates = NULL,
                      args = list(),
                      n.quantile = NULL,
                      right = TRUE,
                      labels = NULL,
                      ...){

  outcome    <- srmisc::select_variable(data, outcome)
  time       <- srmisc::select_variable(data, time)
  exposure   <- srmisc::select_variable(data, exposure)
  covariates <- as.list(covariates)

  if(srmisc::is_empty(covariates)){
    covariates <- list(NULL)
  }else{
    covariates <- lapply(covariates, \(covar){
      srmisc::select_variable(data, covar)
    })
  }

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

  exec <- function(covar){
    if(!is.null(n.quantile) & is.numeric(data[[exposure]]) ){
      data <- srmisc::cut_quantile(data,
                                   varname = exposure,
                                   n = n.quantile,
                                   right = right,
                                   labels = labels)

      res1 <- model(data, x = paste0("gq_", exposure), covar)
      res2 <- model(data, x = paste0("mq_", exposure), covar)
      res2 <- res2[, c(1, ncol(res2)), drop = FALSE]
      res1[1, 1] <- exposure
      res2[1, 1] <- exposure
      names(res2)[2] <- "P for trend"
      srmisc::merge_left(res1, res2, by = names(res1)[1])
    }else{
      model(data, x = exposure, covar)
    }
  }

  results <- lapply(covariates, exec)
  output <- results[[1]]

  if(length(results) > 1L){
    for(i in 2:length(results)){

    }
  }

  output
}
