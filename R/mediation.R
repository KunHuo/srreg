mediation <- function(data,
                      outcome = NULL,
                      time = NULL,
                      exposure = NULL,
                      mediator = NULL,
                      covariates = NULL,
                      args = list(),
                      ...){

  outcome  <- srmisc::select_variable(data, outcome)
  time     <- srmisc::select_variable(data, time)
  exposure <- srmisc::select_variable(data, exposure)
  mediator <- srmisc::select_variable(data, mediator)

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

  covariates <- lapply(covariates, function(covar){
    covar <- setdiff(covar, outcome)
    covar <- setdiff(covar, time)
    covar <- setdiff(covar, exposure)
    covar <- setdiff(covar, mediator)
    if(srmisc::is_empty(covar)){
      NULL
    }else{
      covar
    }
  })

  exec <- function(covar){
    frm.med <- create_formula(dependent = mediator, independents = c(exposure, covar))
    frm.out <- create_formula(dependent = c(time, outcome), independents = c(mediator, exposure, covar))

    if(length(unique(data[[mediator]])) == 2L){
      method.med <- "logit"
    }else{
      method.med <- "linear"
    }

    if(is.null(time)){
      if(length(unique(data[[outcome]])) == 2L){
        method.out <- "logit"
      }else{
        method.out <- "linear"
      }
    }else{
      method.out <- "cox"
    }

    fit.med <- srmisc::do_call(method.med, data = data, formula = frm.med, args)
    fit.out <- srmisc::do_call(method.out, data = data, formula = frm.out, args)

    model <- mediation::mediate(model.m = fit.med,
                       model.y = fit.out,
                       treat = exposure,
                       mediator = mediator,
                       robustSE = TRUE, sims = 100, ...)
    summary(model)

  }

  lapply(covariates, exec)
}
