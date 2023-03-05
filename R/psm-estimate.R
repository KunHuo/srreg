#' Effects for matched data
#'
#' @param object a psmatch object; the output of a call to [psm()].
#' @param model model regression.
#' @param ... further armuments pass to [typeset()].
#'
#' @return a data frame.
#' @export
psm_estimate <- function(object, model = "auto", ...){

  outcome    <- object$outcome
  time       <- object$time
  exposure   <- object$exposure
  positive   <- object$positive
  covariates <- object$covariates

  model <- auto_model(object$data, outcome, time, model)

  res.adj <- associate(data = object$data,
            outcome = outcome,
            time = time,
            exposure = exposure,
            positive = positive,
            covariates = covariates,
            model = model,
            ...)
  res.adj[1, 1] <- "Multivariable model a"

  frm <- create_formula(c(time, outcome), c(exposure))

  data.psm <- psm_data(object)

  if(model == "cox"){
    fit.psm <- srmisc::do_call(model,
                               data = data.psm,
                               formula = frm,
                               positive = positive,
                               weights = data.psm$weights,
                               cluster = data.psm$subclass)

    res.psm <- srmisc::typeset(fit.psm,
                               data = data.psm,
                               outcome = outcome,
                               varnames = exposure,
                               filter = exposure, ...)

  }else{
    if(model == "linear"){
      fit.psm <- srmisc::do_call(model,
                                 data = data.psm,
                                 formula = frm,
                                 weights = data.psm$weights)
      exp <- FALSE
    }else{
      fit.psm <- srmisc::do_call(model,
                                 data = data.psm,
                                 formula = frm,
                                 positive = positive,
                                 weights = data.psm$weights)
      exp <- TRUE
    }

    res.psm <- lmtest::coeftest(fit.psm,
                                vcov. = sandwich::vcovCL,
                                cluster = data.psm$subclass)
    res.psm <- srmisc::typeset(res.psm,
                               data = data.psm,
                               outcome = outcome,
                               varnames = exposure,
                               filter = exposure,
                               exp = exp, ...)
  }
  res.psm[1, 1] <- "Propensity matching b"
  names(res.psm) <- names(res.adj)

  data.iptw <- psm_data(object, type = "iptw")

  if(length(unique(data.iptw[[outcome]])) == 2L){
    data.iptw <- positive_event(data = data.iptw, outcome = outcome, positive)
  }
  res.iptw <- iptw_model(data = data.iptw, frm = frm, model = model)
  res.iptw <- typeset(res.iptw,
                      data = data.iptw,
                      outcome = outcome,
                      varnames = exposure,
                      filter = exposure, ...)
  res.iptw[1, 1] <- "IPTW c"
  res.iptw[2, 2] <- "-"
  res.iptw[3, 2] <- "-"
  out <- rbind(res.adj, res.psm)
  out <- rbind(out, res.iptw)
  names(out)[1] <- "Models"

  abbr <- switch(model,
                 linear  = "Abbreviation: IPTW, inverse probability treatment weighting; CI, confidence interval.",
                 logit   = "Abbreviation: IPTW, inverse probability treatment weighting; OR, odds ratio; CI, confidence interval.",
                 poson   = "Abbreviation: IPTW, inverse probability treatment weighting; RR, risk ratio; CI, confidence interval.",
                 logbinom = "Abbreviation: IPTW, inverse probability treatment weighting; RR, risk ratio; CI, confidence interval.",
                 cox     = "Abbreviation: IPTW, inverse probability treatment weighting; HR, hazard ratio; CI, confidence interval.",
                 "Abbreviation: IPTW, inverse probability treatment weighting; CI, confidence interval.")

  adjusted <- paste(sapply(covariates, function(x){ srmisc::get_var_label(object$data, x, default = ".name", units = FALSE) }),  collapse = ", ")
  note1 <-sprintf("a Adjusted for %s before matching.", adjusted)
  note2 <- "b Propensity score matching with a cluster-robust standard error."
  note3 <- "c IPTW with a cluster-robust standard error."
  notes <- paste(abbr, note1, note2, note3, sep = "\n")

  label.exposure <- srmisc::get_var_label(object$data, exposure, default = ".name", units = FALSE)
  label.outcome  <- srmisc::get_var_label(object$data, outcome,  default = ".name", units = FALSE)

  title <- switch(model,
                  linear  = "multiple linear regression",
                  logit   = "binary logistc regression",
                  poson   = "modified Poissson regression",
                  logbinom = "log-binomial regression",
                  cox     = "Cox proportional hazards regression",
                  default = "")
  title <- sprintf("Table: Association between %s and %s using %s",
                   label.exposure,
                   label.outcome,
                   title)

  attr(out, "title") <- title
  attr(out, "note") <- notes

  out
}


iptw_model <- function(data, frm, model, ...){

   design <- survey::svydesign(ids = ~1, weights = data[["iptw"]], data = data)

  if(model == "linear"){
    out <- survey::svyglm(frm, design = design)
    class(out) <- c("lm")

  }else if(model == "logit"){
    out <- survey::svyglm(frm, design = design, family = stats::quasibinomial) # model
    class(out) <- c("glm")

  }else if(model == "poson"){
    out <- survey::svyglm(frm, design = design, family = stats::poisson(link = "log")) # model
    class(out) <- c("glm")

  }else if(model == "logbinom"){
    out <- survey::svyglm(frm, design = design, family = stats::binomial(link = "log")) # model
    class(out) <- c("glm")

  }else if(model == "cox"){
    out <- survey::svycoxph(frm, design = design)
    class(out) <- c("coxph")
  }
  out
}
