subgroup <- function(data,
                    outcome = NULL,
                    time = NULL,
                    exposure = NULL,
                    covariates = NULL,
                    strata = NULL,
                    args = list(),
                    p.trend = TRUE,
                    n.quantile = NULL,
                    quantile.right = TRUE,
                    quantile.labels = NULL,
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
                select = c("effect", "p"), ...)
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
                       select = c("effect"),
                       ...)
      res[-1, , drop = FALSE]
    }, warning = FALSE)

    if(!is.null(gres)){
      gres$id <- rep(1:length(unique(data[[svar]])),
                     each = length(unique(data[[exposure]])))
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
                       select = c("effect"),
                       ...)
      res[-1, , drop = FALSE]
    }, warning = FALSE)

    if(!is.null(gres)){
      gres$id <- rep(1:length(unique(data[[svar]])),
                     each = length(unique(data[[paste0("gq_", exposure)]])))
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
                    select = c("p.value"),
                    ...)
        }, warning = FALSE)

        if(!is.null(trend)){
          trend <- trend[, -2, drop = FALSE]
          names(trend)[2] <- "P for trend"
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
                     args = args)

  output <- srmisc::merge_left(output, results, by = "term")
  output <- srmisc::merge_left(output, LRT.results, by = "term")
  output <- output[, -c(1:3), drop = FALSE]
  names(output)[1] <- "Subgroup"
  output
}




