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
  strata <- setdiff(strata, outcome)
  strata <- setdiff(strata, exposure)

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
    gres <- gres[, -2, drop = FALSE]
    gres
  }

  exec3 <- function(svar){
    gres <- srmisc::group_exec(data, group = svar, func = function(d){
      d <-  droplevels.data.frame(d)
      res <- associate(data = d,
                       outcome = outcome,
                       time = time,
                       exposure = exposure,
                       covariates = setdiff(covariates, svar),
                       select = c("effect"), ...)
      res <- res[-1, , drop = FALSE]
      res
    })

    gres$id <- rep(1:length(unique(data[[svar]])), each = length(unique(data[[exposure]])))

    gres <- srmisc::reshape_wide(gres, id = "id", names.from = 2, values.from = 3, include.id = FALSE)
    gres
  }

  output <- lapply(strata, exec3)

  output <- lapply(output, function(x){
    x[[1]] <- paste(names(x)[1], x[[1]], sep = "")
    names(x)[1] <- "term"
    x
  })
  output <- do.call(rbind, output)
  output <- srmisc::merge_left(srmisc::fmt_reg(data = data, varnames = strata), output, by = "term")
  output <- output[, -c(1:3), drop = FALSE]
  names(output)[1] <- "Subgroup"
  output


  srmisc::describe_event(data = data, event = outcome, varnames = strata, method = "n.event.total")
}

