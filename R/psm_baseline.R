psm_baseline <- function(object,
                         psm = TRUE,
                         iptw = TRUE,
                         digits.smd = 3,
                         digits.numeric = 2,
                         digits.category = 1, ...){

  res.unmatch <- srr2bline::baseline(data = psm_data(object, type = "unmatched"),
                             group = object$exposure,
                             varnames = object$covariates,
                             show.test = FALSE,
                             show.smd = TRUE,
                             show.overall = FALSE,
                             digits.pvalue = digits.smd,
                             digits.numeric = digits.numeric,
                             digits.category = digits.category,
                             ...)
  if(psm){
    res.psm <-  srr2bline::baseline(data = psm_data(object, type = "psm"),
                                    group = object$exposure,
                                    varnames = object$covariates,
                                    show.test = FALSE,
                                    show.smd = TRUE,
                                    show.overall = FALSE,
                                    digits.pvalue = digits.smd,
                                    digits.numeric = digits.numeric,
                                    digits.category = digits.category,
                                    ...)
    out <- srmisc::merge_table(res.unmatch,
                               res.psm,
                               name.x = "Before matching",
                               name.y = "After PS matching")
  }

  if(iptw){
    dat <- psm_data(object, type = "iptw")
    svydat <- survey::svydesign(ids = ~ 1, data = dat, weights = ~ iptw)
    res.iptw  <- tableone::svyCreateTableOne(vars = object$covariates,
                                             data = svydat,
                                             strata = object$exposure,
                                             test = FALSE,
                                             smd = TRUE)
    res.iptw <- svyTable(res.iptw,
                         digits.pvalue = digits.smd,
                         digits.numeric = digits.numeric,
                         digits.category = digits.category)

    res.iptw <- srmisc::merge_left(srmisc::fmt_reg(object$data, varnames = object$covariates),
                       res.iptw,
                       by = "term")
    res.iptw <- res.iptw[, -c(1, 2, 3)]
    names(res.iptw)[1] <- "Variable"

    if(psm){
      out <- srmisc::merge_table(out, res.iptw, name.y = "After IPTW matching")
    }else{
      out <- srmisc::merge_table(res.unmatch,
                                 res.iptw,
                                 name.x = "Before matching",
                                 name.y = "After IPTW matching")
    }
  }

  names(out)[1] <- "Characteritics"

  class(out) <- c("srreg", "data.frame")
  out
}


svyTable <- function(res,
                     digits.category = 1,
                     digits.numeric = 2,
                     digits.pvalue = 3,
                     method.normal = "mean_sd",
                     method.nonnormal = "median_IQR",
                     varnames.nonnormal = NULL,
                     method.category = "nprop"){

  res.cont <- svyContTable(res,
                           digits.numeric = digits.numeric,
                           digits.pvalue = digits.pvalue,
                           method.normal = method.normal,
                           method.nonnormal = method.nonnormal,
                           varnames.nonnormal = varnames.nonnormal)

  res.cat <- svyCatTable(res,
                         digits.category = digits.category,
                         digits.pvalue = digits.pvalue,
                         method.category = method.category)

  out <- rbind(res.cont, res.cat)
  out
}


svyContTable <- function(res,
                         digits.numeric = 2,
                         digits.pvalue = 3,
                         method.normal = "mean_sd",
                         method.nonnormal = "median_IQR",
                         varnames.nonnormal = NULL){

  if(is.null(res$ContTable)){
    return(NULL)
  }

  smd <- as.data.frame(attr(res$ContTable, "smd"))
  names(smd) <- "SMD"
  smd <- srmisc::rownames_to_column(smd)
  smd$SMD <- srmisc::fmt_digits(smd$SMD, digits.pvalue)

  out <- srmisc::list_flatten(res$ContTable)

  out <- Map(function(d, nm){
    d <- as.data.frame(d)
    term <- row.names(d)
    desc.normal <- sprintf("%.2f\u00b1%.2f", d$mean, d$sd)
    desc.nonnormal <- sprintf("%.2f (%.2f, %.2f)", d$median, d$p25, d$p75)
    desc <- ifelse(term %in% varnames.nonnormal, desc.nonnormal, desc.normal)

    o <- data.frame(term = term, desc = desc)
    names(o)[2] <- nm
    o
  }, out, names(out))

  out <- srmisc::merge_left(out[[1]], out[[2]], by = "term")
  out <- srmisc::merge_left(out, smd, by = "term")
  out
}


svyCatTable <- function(res, digits.category = 1, digits.pvalue = 3, method.category = "nprop"){
  if(is.null(res$CatTable)){
    return(NULL)
  }

  smd <- as.data.frame(attr(res$CatTable, "smd"))
  names(smd) <- "SMD"
  smd <- srmisc::rownames_to_column(smd)
  smd$SMD <- srmisc::fmt_digits(smd$SMD, digits.pvalue)

  out <- Map(function(dat, nms){
    dat <- srmisc::list_flatten(dat)
    dat <- Map(function(d, nm){
      d <- as.data.frame(d)
      d$n <- round(d$n, 0)
      d$freq <- d$freq
      d$percent <- d$freq / d$n * 100
      term <- c(nm, paste0(nm, d$level))
      desc <- sprintf("%.1f (%s%%)", d$freq, srmisc::fmt_digits(d$percent, digits.category))
      desc <- c(NA, desc)

      data.frame(term, desc)
    }, dat, names(dat))

    dat <- do.call(rbind, dat)
    row.names(dat) <- NULL
    names(dat)[2] <- nms
    dat

  }, res$CatTable, names(res$CatTable))

  out <- srmisc::merge_left(out[[1]], out[[2]], by = "term")
  out <- srmisc::merge_left(out, smd, by = "term")
  out
}
