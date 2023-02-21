subgroup <- function(data,
                    outcome = NULL,
                    time = NULL,
                    exposure = NULL,
                    covariates = NULL,
                    strata = NULL,
                    args = list(), ...){

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

  srmisc::fmt_reg(data = data, varnames = strata)

  # P.LRT <- lapply(strata, function(sg){
  #   adjusted <- setdiff(adjusted, sg)
  #   frm1 <- create_formula(self$dependent,  c(exposure, sg, adjusted))
  #   frm2 <- create_formula2(self$dependent, exposure, sg, adjusted)
  #
  #   model1 <- private$model(formula = frm1, data = self$data)
  #   model2 <- private$model(formula = frm2, data = self$data)
  #
  #   pvalue <- stats::anova(model1, model2, test = "LRT")
  #   pvalue <- format_pvalue(pvalue[, ncol(pvalue)][2], digits = private$.digits.pvalue)
  #   res <- data.frame(term = sg, pvalue)
  #   names(res)[2] <- "P for interaction"
  #   res
  # })

  exec2 <- function(svar){
    gres <- group_exec(data, group = svar, func = function(d){
      d <-  droplevels.data.frame(d)
      res <- associate(data = d,
                outcome = outcome,
                time = time,
                exposure = exposure,
                covariates = setdiff(covariates, svar),
                select = c("effect", "p"))
      res
    })
    # names(gres)[1] <- "subgroup"
    gres
  }

  exec3 <- function(svar){
    gres <- group_exec(data, group = svar, func = function(d){
      d <-  droplevels.data.frame(d)
      res <- associate(data = d,
                       outcome = outcome,
                       time = time,
                       exposure = exposure,
                       covariates = setdiff(covariates, svar),
                       select = c("effect", "p"))
      res
    })
    gres
  }

  lapply(strata, exec2)
}

