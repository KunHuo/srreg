match_baseline <- function(object, type = c("unmatched", "psm", "iptw"), ...){

  res.unmatch <- srr2bline::baseline(data = match_data(object, type = "unmatched"),
                             group = object$exposure,
                             varnames = object$covariates,
                             show.test = FALSE,
                             show.smd = TRUE,
                             show.overall = FALSE,
                             ...)

  res.psm <-  srr2bline::baseline(data = match_data(object, type = "psm"),
                                  group = object$exposure,
                                  varnames = object$covariates,
                                  show.test = FALSE,
                                  show.smd = TRUE,
                                  show.overall = FALSE,
                                  ...)

  dat <- match_data(object, type = "iptw")
  svydat <- survey::svydesign(ids = ~ 1, data = dat, weights = ~ iptw)
  res  <- tableone::svyCreateTableOne(vars = object$covariates,
                                     data = svydat,
                                     strata = object$exposure,
                                     test = FALSE,
                                     smd = TRUE)
  svyTable(res)
  # out <- srr2misc::merge_left(srr2misc::format_variable(self$data, varnames = self$covariates),
  #                             svyTable(res),
  #                             by = "term")
  # out <- out[, -c(1:3)]
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
  smd <- srr2misc::rownames_to_column(smd)
  smd$SMD <- srr2misc::format_digits(smd$SMD, digits.pvalue)

  out <- srr2misc::flatten_list(res$ContTable)

  out <- Map(function(d, nm){
    d <- as.data.frame(d)
    term <- row.names(d)
    desc.normal <- sprintf("%.2f (%.2f)", d$mean, d$sd)
    desc.nonnormal <- sprintf("%.2f (%.2f, %.2f)", d$median, d$p25, d$p75)
    desc <- ifelse(term %in% varnames.nonnormal, desc.nonnormal, desc.normal)

    o <- data.frame(term = term, desc = desc)
    names(o)[2] <- nm
    o
  }, out, names(out))

  out <- srr2misc::merge_left(out[[1]], out[[2]], by = "term")
  out <- srr2misc::merge_left(out, smd, by = "term")
  out
}


svyCatTable <- function(res, digits.category = 1, digits.pvalue = 3, method.category = "nprop"){
  if(is.null(res$CatTable)){
    return(NULL)
  }

  smd <- as.data.frame(attr(res$CatTable, "smd"))
  names(smd) <- "SMD"
  smd <- srr2misc::rownames_to_column(smd)
  smd$SMD <- srr2misc::format_digits(smd$SMD, digits.pvalue)

  out <- Map(function(dat, nms){
    dat <- srr2misc::flatten_list(dat)
    dat <- Map(function(d, nm){
      d <- as.data.frame(d)
      d$n <- round(d$n, 0)
      d$freq <- round(d$freq, 0)
      d$percent <- d$freq / d$n * 100
      term <- c(nm, paste0(nm, d$level))
      desc <- sprintf("%d (%s%%)", d$freq, srr2misc::format_digits(d$percent, digits.category))
      desc <- c(NA, desc)

      data.frame(term, desc)
    }, dat, names(dat))

    dat <- do.call(rbind, dat)
    row.names(dat) <- NULL
    names(dat)[2] <- nms
    dat

  }, res$CatTable, names(res$CatTable))

  out <- srr2misc::merge_left(out[[1]], out[[2]], by = "term")
  out <- srr2misc::merge_left(out, smd, by = "term")
  out
}
