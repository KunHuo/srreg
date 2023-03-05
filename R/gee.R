gee_change <- function(data,
                outcome = NULL,
                exposure = NULL,
                covariates = NULL,
                id = NULL,
                family = gaussian(),
                corstr = "independence",
                ...){

  if("taskreg" %in% class(data)){
    if(is.null(outcome)){
      outcome <- data$outcome
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

    data <- data$data
  }else{
    outcome    <- srmisc::select_variable(data, outcome)
    exposure   <- srmisc::select_variable(data, exposure)
  }

  if(srmisc::is_empty(covariates)){
    covariates <- list(NULL)
  }else{
    if(is.list(covariates)){
      covariates <- lapply(covariates, \(covar){
        covar <- srmisc::select_variable(data, covar)
        covar <- setdiff(covar, outcome)
        covar <- setdiff(covar, exposure)
        covar
      })
    }else{
      covariates <- srmisc::select_variable(data, covariates)
      covariates <- setdiff(covariates, outcome)
      covariates <- setdiff(covariates, exposure)
      covariates <- list(covariates)
    }
  }

  if(is.null(id)){
    stop("The 'id' can not be NULL. Which is a vector which identifies the clusters", call. = FALSE)
  }

  data[[exposure]] <- factor(data[[exposure]])

  desc <- lapply(outcome[-1], \(x){
    dat <- data.frame(diff = data[[x]] - data[[outcome[[1]]]], group = data[[exposure]])

    res <- srmisc::group_exec(dat, group = "group", func = function(d){
      data.frame(mean = sprintf("%s (%s)",
                                srmisc::fmt_digits(mean(d[["diff"]]), 2),
                                srmisc::fmt_digits(stats::sd(d[["diff"]]), 2) ))
    })
    res <- srmisc::transpose(res)
    res[-1]
  })

  desc <- srmisc::list_rbind(desc)
  names(desc)[1] <- "outcome"

  if(length(outcome) > 2L){
    data <- srmisc::reshape_long(data,
                             cols = outcome,
                             names.to = ".time",
                             values.to = ".value",
                             include.id = FALSE)
  }

  data <- data[order(data[[id]]), ]

  exec <- function(covar){
    frm <- paste(exposure, ".time", sep = " * ")
    frm <- paste(".value", frm, sep = " ~ ")
    if(!srmisc::is_empty(covar)){
      frm <- paste(frm, paste(covar, collapse = " + "), sep = " + ")
    }
    frm <- stats::as.formula(frm)

    fit <- geepack::geeglm(formula = frm,
                    family = family,
                    data = data,
                    id = data[[id]],
                    corstr = corstr)

    res <- srmisc::typeset(fit, select = c("effect", "p"), ...)
    res <- res[srmisc::regex_detect(res[[1]], ":", fixed = TRUE), ]
    res <- srmisc::separate2cols(res,
                                 varname = "Variable",
                                 into = c(exposure, "outcome"),
                                 sep = ":",
                                 fixed = TRUE)

    if(length(unique(data[[exposure]])) > 2L){
      res[[3]] <- fmt_signif(res[[3]], res[[4]])
      res <- res[-4]
    }

    res[["outcome"]] <- srmisc::regex_replace(res[["outcome"]],
                                              pattern = ".time",
                                              replacement = "",
                                              fixed = TRUE)

    res[[exposure]] <- paste(res[[exposure]], levels(data[[exposure]])[1], sep = " vs. ")
    res$id <- rep(1:(length(outcome) - 1),
                  each = length(unique(data[[exposure]])) - 1)
    res <- srmisc::reshape_wide(res,
                                id = "id",
                                names.from = 1,
                                values.from = 3,
                                include.id = FALSE)
    res
  }

  out <- lapply(covariates, exec)

  out <- do.call(rbind, out)

  srmisc::merge_left(desc, out, by = "outcome")

}


fmt_signif <- function(text, pvalues){
    pvalues.numeric <- as.numeric(gsub(pattern = "<|>", replacement = "", x = pvalues))
    sign <- ifelse(pvalues.numeric <= 0.001, "***",
                   ifelse(pvalues.numeric <= 0.01, "**",
                          ifelse(pvalues.numeric <= 0.05, "*",
                                 ifelse(pvalues.numeric <= 0.1, ".", ""))))
    sign <- ifelse(is.na(sign), "", format(sign, justify = "left"))
    pvalues <- ifelse(is.na(pvalues), "", format(pvalues, justify = "right"))
    paste(text, sign, sep = " ")
}



