#
# surv_median <- function(data, event = NULL, time = NULL, independents = NULL,
#                         exclude = NULL,
#                         positive = NULL,
#                         digits.median = 1,
#                         digits.pvalue = 3,
#                         fmt = NULL,
#                         test = TRUE,
#                         rho = 0,
#                         show.stat = FALSE,
#                         show.event.number = TRUE,
#                         show.total.number = TRUE){
#
#   data <- positive_event(data = data, varname = event, positive = positive)
#
#
#   if(!is.null(independents)){
#     if(independents[1] == "ALL"){
#       independents <- names(data)[sapply(data, function(x){ is.factor(x) | is.character(x)})]
#     }
#   }
#   independents <- setdiff(independents, event)
#   independents <- setdiff(independents, time)
#   independents <- setdiff(independents, exclude)
#
#   execute <- function(x){
#     frm <- sprintf("survival::Surv(%s, %s) ~ %s", time, event, x)
#     frm <- stats::as.formula(frm)
#     fit <- survival::survfit(formula = frm, data = data)
#     fit
#   }
#
#   format_median <- function(res){
#     res[[3]] <- sprintf(fmt,
#                         format_digits(res[[3]], digits = digits.median),
#                         format_digits(res[[4]], digits = digits.median),
#                         format_digits(res[[5]], digits = digits.median))
#     res <- res[, -c(4, 5), drop = FALSE]
#     res
#   }
#
#   if(is.null(fmt)){
#     fmt <- "%s (%s\u2013%s)"
#   }
#
#   if(is.null(independents)){
#     out <- execute("1")
#     out <- print_survfit(out)
#     out <- as.list(out)
#     out <- as.data.frame(out)
#     out <- format_median(out)
#     names(out) <- c("n", "No. of event", "Median time (95% CI)")
#     if(!show.event.number){
#       out <- out[, -2, drop = FALSE]
#     }
#     if(!show.total.number){
#       out <- out[, -1, drop = FALSE]
#     }
#     out
#   }else{
#     out <- lapply(independents, function(x){
#       res <- execute(x)
#       res <- print_survfit(res)
#       res <- as.data.frame(res)
#       res <- format_median(res)
#       term <- regex_replace(row.names(res), "=", "", fixed = TRUE)
#       variable <- regex_replace(row.names(res), paste0(x, "="), "", fixed = TRUE)
#       variable <- paste0("    ", variable)
#       res <- cbind(data.frame(term, variable), res)
#       res <- rbind(NA, res)
#       res[1, 1] <- x
#       res[1, 2] <- get_label(data, x)
#       row.names(res) <- NULL
#       res
#     })
#     out <- do.call(rbind, out)
#
#     if(test){
#       lk <- log_rank(data = data, event = event, time = time, independents = independents, exclude, digits.pvalue = digits.pvalue, rho = rho)
#       lk <- cbind(attr(lk, "term"), lk)
#       lk <- lk[, -2]
#       out <- merge_left(out, lk, by = "term")
#       term <- out[1]
#       out <- out[-1]
#       names(out) <- c("Variable", "n", "No. of event", "Median time (95% CI)", "Chisq", "P value")
#       if(!show.stat){
#         out <- out[, -which(names(out) == "Chisq")]
#       }
#       if(!show.event.number){
#         out <- out[, -3, drop = FALSE]
#       }
#       if(!show.total.number){
#         out <- out[, -2, drop = FALSE]
#       }
#       attr(out, "term") <- term
#       attr(out, "title") <- "Median survival time"
#       class(out) <- c("mst", class(out))
#       out
#     }
#   }
# }
#
#
# surv_rate <- function(data, event = NULL, time = NULL, independents = NULL, point = NULL,
#                       exclude = NULL,
#                       positive = NULL){
#
#   data <- positive_event(data = data, varname = event, positive = positive)
#
#   if(!is.null(independents)){
#     if(independents[1] == "ALL"){
#       independents <- names(data)[sapply(data, function(x){ is.factor(x) | is.character(x)})]
#     }
#   }
#   independents <- setdiff(independents, event)
#   independents <- setdiff(independents, time)
#   independents <- setdiff(independents, exclude)
#
#
#   execute <- function(x){
#     frm <- sprintf("survival::Surv(%s, %s) ~ %s", time, event, x)
#     frm <- stats::as.formula(frm)
#     fit <- survival::survfit(formula = frm, data = data)
#     fit
#   }
#
#
#   if(is.null(independents)){
#     out <- execute("1")
#     out <- summary(out, point)
#     out
#   }else{
#     out <- lapply(independents, function(x){
#       res <- execute(x)
#       res <- summary(res, point)
#       res
#     })
#     out
#   }
#
#
# }
#
#
#
#
#
#
# log_rank <- function(data, event = NULL, time = NULL, independents = NULL, exclude = NULL, positive = NULL, rho = 0, digits.pvalue = 3){
#   data <- positive_event(data = data, varname = event, positive = positive)
#
#   if(is.null(independents)){
#     independents <- names(data)[sapply(data, function(x){ is.factor(x) | is.character(x)})]
#   }
#   independents <- setdiff(independents, event)
#   independents <- setdiff(independents, time)
#   independents <- setdiff(independents, exclude)
#
#   execute <- function(x){
#     frm <- sprintf("survival::Surv(%s, %s) ~ %s", time, event, x)
#     frm <- stats::as.formula(frm)
#     fit <- survival::survdiff(formula = frm, data = data, rho = rho)
#     chisq <- format_pvalue(fit$chisq, digits.pvalue)
#     pvalue <- format_pvalue(stats::pchisq(fit$chisq, length(fit$n) - 1, lower.tail = FALSE), digits = digits.pvalue)
#     data.frame(term = x, Variable = get_label(data, x), chisq = chisq, P = pvalue, stringsAsFactors = FALSE)
#   }
#   out <- lapply(independents, execute)
#   out <- do.call(rbind, out)
#
#   if(rho == 0){
#     title <- "Log-rank test"
#   }else{
#     title <- "Gehan-Wilcoxon test"
#   }
#
#   term <- out[, 1, drop = FALSE]
#   out <- out[, -1, drop = FALSE]
#   names(out) <- c("Variable", "Chisq", "P value")
#   attr(out, "term") <- term
#   attr(out, "title") <- title
#   class(out) <- c("logrank", class(out))
#   out
# }

km <- function(data,
               outcome = NULL,
               time = NULL,
               exposure = NULL,
               covariates = NULL,
               positive = "auto",
               ...){

  if("taskreg" %in% class(data)){
    if(is.null(outcome)){
      outcome <- data$outcome
    }
    if(is.null(time)){
      time <- data$time
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

    model <- match.arg(model)
    if(model == "auto"){
      model <- data$model
    }
    data <- data$data
  }else{
    outcome    <- srmisc::select_variable(data, outcome)
    time       <- srmisc::select_variable(data, time)
    exposure   <- srmisc::select_variable(data, exposure)
    covariates <- srmisc::select_variable(data, covariates)
  }

  data <- positive_event(data, outcome, positive)
  varnames <- unique(c(exposure, covariates))

  extract_median <- function(fit){
    res.median <- print_survfit(fit)
    if(srmisc::is_empty(varnames)){
      res.median <- as.data.frame(as.list(res.median))
    }else{
      res.median <- as.data.frame(res.median)

    }
    res.median <- srmisc::rownames_to_column(res.median)
    res.median
  }

  if(srmisc::is_empty(varnames)){
    frms <- list(create_formula(c(time, outcome)))
  }else{
    frms <- lapply(varnames, \(x) create_formula(c(time, outcome), x))
  }

  exec <- function(frm){
    fit <- survival::survfit(frm, data = data)
    surv.median <- extract_median(fit)
  }

  lapply(frms, exec)


}

print_survfit <- function(x, scale=1,
                          digits = max(options()$digits - 4, 3),
                          print.rmean = getOption('survfit.print.rmean'),
                          rmean = getOption('survfit.rmean'), ...) {

  if (!is.null(cl<- x$call)) {
    # cat("Call: ")
    # dput(cl)
    # cat("\n")
  }
  omit <- x$na.action
  if (length(omit)) cat("  ", stats::naprint(omit), "\n")

  savedig <- options(digits=digits)
  on.exit(options(savedig))

  #   x <- survfit32(x)
  # The print.rmean option is depreciated, with the more general
  #   rmean option taking its place.  But if someone specifically
  #   uses print.rmean in the call, or has it as an option without
  #   the rmean option, listen to them.
  if (!missing(print.rmean) && is.logical(print.rmean) && missing(rmean)) {
    if (print.rmean) rmean <- 'common'
    else             rmean <- 'none'
  }

  else {
    if (is.null(rmean)) {
      if (is.logical(print.rmean)) {
        if (print.rmean) rmean <- 'common'
        else             rmean <- 'none'
      }
      else rmean <- 'none'  #no option set
    }

    # Check validity: it can be numeric or character
    if (is.numeric(rmean)) {
      if (is.null(x$start.time)) {
        if (rmean < min(x$time))
          stop("Truncation point for the mean is < smallest survival")
      }
      else if (rmean < x$start.time)
        stop("Truncation point for the mean is < smallest survival")
    }
    else {
      rmean <- match.arg(rmean, c('none', 'common', 'individual'))
      if (length(rmean)==0) stop("Invalid value for rmean option")
    }
  }

  temp <- survmean(x, scale=scale, rmean)
  # If the first columns of survmean are identical, suppress duplicates
  #
  mtemp <- if (is.matrix(temp$matrix)) temp$matrix
  else matrix(temp$matrix, nrow=1,
              dimnames=list(NULL, names(temp$matrix)))
  if (all(mtemp[,2] == mtemp[,3])){
    cname <- dimnames(mtemp)[[2]]
    mtemp <- mtemp[,-2, drop=FALSE]
    cname <-cname[-2]
    cname[2] <- "n"
    dimnames(mtemp)[[2]] <- cname
  }

  if (all(mtemp[,1] == mtemp[,2]))
    mtemp <- mtemp[,-1, drop=FALSE]
  temp$matrix <- drop(mtemp)

  temp$matrix

  # print(temp$matrix)
  # if (rmean != 'none') {
  #   if (rmean == 'individual')
  #     cat("   * restricted mean with variable upper limit\n")
  #   else cat("    * restricted mean with upper limit = ",
  #            format(temp$end.time[1]), "\n")
  # }
  # invisible(x)
}


#
# The function that does all of the actual work -- output is a matrix
#   Used by both print.survfit and summary.survfit
#
survmean <- function(x, scale=1, rmean) {

  # The starting point for the integration of the AUC
  if (!is.null(x$start.time)) start.time <- x$start.time
  else                        start.time <- min(0, x$time)

  #
  # The function below is called once for each line of output,
  #  i.e., once per curve.  It creates the line of output
  #
  pfun <- function(nused, time, surv, n.risk, n.event, lower, upper,
                   start.time, end.time) {
    #
    # Start by defining a small utility function
    # Multiple times, we need to find the x corresponding to the first
    #    y that is <.5.  (The y's are in decreasing order, but may have
    #    duplicates).
    # Nuisance 1: if one of the y's is exactly .5, we want the mean of
    #    the corresponding x and the first x for which y<.5.  We need to
    #    use the equivalent of all.equal to check for a .5 however:
    #    survfit(Surv(1:100)~1) gives a value of .5 + 1.1e-16 due to
    #    roundoff error.
    # Nuisance 2: there may by an NA in the y's
    # Nuisance 3: if no y's are <=.5, then we should return NA
    # Nuisance 4: the obs (or many) after the .5 may be censored, giving
    #   a stretch of values = .5 +- epsilon
    #
    minmin <- function(y, x) {
      tolerance <- .Machine$double.eps^.5   #same as used in all.equal()
      keep <- (!is.na(y) & y <(.5 + tolerance))
      if (!any(keep)) NA
      else {
        x <- x[keep]
        y <- y[keep]
        if (abs(y[1]-.5) <tolerance  && any(y< y[1]))
          (x[1] + x[min(which(y<y[1]))])/2
        else x[1]
      }
    }

    # compute the mean of the curve, with "start.time" as 0
    #   start by drawing rectangles under the curve
    # Lining up the terms for "varmean" is tricky -- the easiest
    #   check is to look at the homework solution on page 195-196
    #   of Miller, Survival Analysis, Wiley, 1981.
    # Computing the mean sojourn time for a multi-state curve is
    #  a completely different task.
    if (!is.na(end.time)) {
      hh <- ifelse((n.risk-n.event)==0, 0,
                   n.event /(n.risk *(n.risk -n.event)))
      keep <- which(time <= end.time)

      if (length(keep) ==0) { # the cutoff is before the first event
        temptime <- end.time
        tempsurv <- 1
        hh <- 0
      }
      else {
        temptime <- c(time[keep], end.time)
        tempsurv <- c(surv[keep], surv[max(keep)])
        hh <- c(hh[keep], 0)
      }
      n <- length(temptime)
      delta <- diff(c(start.time, temptime))     #width of rectangles
      rectangles <- delta * c(1, tempsurv[-n])   #area of rectangles
      varmean <- sum( cumsum(rev(rectangles[-1]))^2 * rev(hh)[-1])
      mean <- sum(rectangles) + start.time
    }
    else {
      mean <- 0
      varmean <- 0  #placeholders
    }

    #compute the median  and ci(median)
    med <- minmin(surv, time)
    if (!is.null(upper)) {
      upper <- minmin(upper, time)
      lower <- minmin(lower, time)
      c(nused, max(n.risk), n.risk[1],
        sum(n.event), sum(mean), sqrt(varmean), med, lower, upper)
    }
    else
      c(nused, max(n.risk), n.risk[1], sum(n.event),
        sum(mean), sqrt(varmean), med, 0, 0)
  }


  # Now to the actual work
  #  We create an output matrix with all 9 columns, and then trim some
  #  out at the end.
  #  If rmean='none' for instance, pfun above returns dummy values for
  #    the mean and var(mean).  Similar for CI of the median.
  #
  stime <- x$time/scale
  if (is.numeric(rmean)) rmean <- rmean/scale
  surv <- x$surv
  plab <- c("records", "n.max", "n.start", "events",
            "*rmean", "*se(rmean)", "median",
            paste(x$conf.int, c("LCL", "UCL"), sep=''))  #col labels
  ncols <- 9    #number of columns in the output


  #Four cases: strata Y/N  by  ncol(surv)>1 Y/N
  #  Repeat the code, with minor variations, for each one
  if (is.matrix(surv) && !is.matrix(x$n.event))  #make it easier
    x$n.event <- matrix(rep(x$n.event, ncol(surv)), ncol=ncol(surv))

  if (is.null(x$strata)) {
    if (rmean=='none') end.time <- NA
    else if (is.numeric(rmean)) end.time <- rmean
    else end.time <- max(stime)

    if (is.matrix(surv)) {
      out <- matrix(0, ncol(surv), ncols)
      for (i in 1:ncol(surv)) {
        if (is.null(x$conf.int))
          out[i,] <- pfun(x$n, stime, surv[,i], x$n.risk,
                          x$n.event[,i],
                          NULL, NULL, start.time, end.time)
        else out[i,] <- pfun(x$n, stime, surv[,i], x$n.risk,
                             x$n.event[,i],
                             x$lower[,i], x$upper[,i], start.time,
                             end.time)
      }
      dimnames(out) <- list(dimnames(surv)[[2]], plab)
    }
    else {
      out <- matrix(pfun(x$n, stime, surv, x$n.risk, x$n.event, x$lower,
                         x$upper, start.time, end.time), nrow=1)
      dimnames(out) <- list(NULL, plab)
    }
  }
  else {   #strata case
    nstrat <- length(x$strata)
    stemp <- rep(1:nstrat,x$strata)  # the index vector for strata1, 2, etc

    last.time <- (rev(stime))[match(1:nstrat, rev(stemp))]
    if (rmean=='none') end.time <- rep(NA, nstrat)
    else if (is.numeric(rmean)) end.time <- rep(rmean, nstrat)
    else if (rmean== 'common')  end.time <- rep(max(last.time), nstrat)
    else end.time <- last.time
    if (is.matrix(surv)) {
      ns <- ncol(surv)
      out <- matrix(0, nstrat*ns, ncols)
      if (is.null(dimnames(surv)[[2]]))
        dimnames(out) <- list(rep(names(x$strata), ns),
                              plab)
      else {
        cname <- outer(names(x$strata), dimnames(surv)[[2]], paste,
                       sep=", ")
        dimnames(out) <- list(c(cname), plab)
      }
      k <- 0
      for (j in 1:ns) {
        for (i in 1:nstrat) {
          who <- (stemp==i)
          k <- k+1
          if (is.null(x$lower))
            out[k,] <- pfun(x$n[i], stime[who], surv[who,j],
                            x$n.risk[who], x$n.event[who,j],
                            NULL, NULL, start.time, end.time[i])
          else out[k,] <- pfun(x$n[i], stime[who], surv[who,j],
                               x$n.risk[who], x$n.event[who,j],
                               x$lower[who,j], x$upper[who,j],
                               start.time, end.time[i])
        }
      }
    }
    else { #non matrix case
      out <- matrix(0, nstrat, ncols)
      dimnames(out) <- list(names(x$strata), plab)
      for (i in 1:nstrat) {
        who <- (stemp==i)
        if (is.null(x$lower))
          out[i,] <- pfun(x$n[i], stime[who], surv[who],
                          x$n.risk[who], x$n.event[who],
                          NULL, NULL, start.time, end.time[i])
        else out[i,] <- pfun(x$n[i], stime[who], surv[who],
                             x$n.risk[who], x$n.event[who],
                             x$lower[who], x$upper[who], start.time,
                             end.time[i])
      }
    }
  }

  if (is.null(x$lower)) out <- out[,1:7, drop=F]   #toss away the limits
  if (rmean=='none') out <- out[,-(5:6), drop=F]   #toss away the mean & sem
  list(matrix=out[,,drop=T], end.time=end.time)
}

