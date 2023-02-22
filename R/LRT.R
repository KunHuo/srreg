LRT <- function(data, outcome, time, exposure, covariates, strata, args = list(), digits.pvalue = 3){

  output <- lapply(strata, \(svar){
    covariates <- setdiff(covariates, svar)
    if(srmisc::is_empty(covariates)){
      covariates <- NULL
    }

    frm1 <- create_formula(c(time, outcome), c(exposure, svar, covariates))
    frm2 <- create_formula2(c(time, outcome), exposure, svar, covariates)

    if(is.null(time)){
      if(length(unique(data[[outcome]])) == 2L){
        fit1 <- srmisc::do_call(logit, data = data, formula = frm1, args)
        fit2 <- srmisc::do_call(logit, data = data, formula = frm2, args)
      }else{
        if(is.numeric(data[[outcome]])){
          fit1 <- srmisc::do_call(linear, data = data, formula = frm1, args)
          fit2 <- srmisc::do_call(linear, data = data, formula = frm2, args)
        }
      }
    }else{
      fit1 <- srmisc::do_call(cox, data = data, formula = frm1, args)
      fit2 <- srmisc::do_call(cox, data = data, formula = frm2, args)
    }

    pvalue <- stats::anova(fit1, fit2, test = "LRT")
    pvalue <- srmisc::fmt_pvalue(pvalue[, ncol(pvalue)][2], digits = digits.pvalue)
    res <- data.frame(term = svar, pvalue)
    names(res)[2] <- "P for interaction"
    res
  })
  do.call(rbind, output)
}
