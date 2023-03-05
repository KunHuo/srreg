#' Effects for matched data
#'
#' @param object a psmatch object; the output of a call to [psm()].
#' @param model model regression.
#' @param ... further armuments.
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
  res.adj[1, 1] <- "Adjusted model"

  frm <- create_formula(c(time, outcome), c(exposure))

  data.psm <- psm_data(object)

  if(model == "cox"){

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
                               exp = exp)
    res.psm[1, 1] <- "Propensity matching"
    names(res.psm) <- names(res.adj)
  }

  data.iptw <- psm_data(object, type = "iptw")
  data.iptw <- positive_event(data = data.iptw, outcome = outcome, positive)
  res.iptw <- iptw_model(data = data.iptw, frm = frm, model = model)
  res.iptw <- typeset(res.iptw,
                      data = data.iptw,
                      outcome = outcome,
                      varnames = exposure,
                      filter = exposure)
  res.iptw
  # res.iptw[1, 1] <- "IPTW"
  # out <- rbind(res.adj, res.psm)
  # out <- rbind(out, res.iptw)
  # out
}


iptw_model <- function(data, frm, model, ...){

   design <- survey::svydesign(ids = ~1, weights = data[["iptw"]], data = data)

  if(model == "linear"){
    out <- survey::svyglm(frm, design = design)
    class(out) <- c("lm")

  }else if(model == "logit"){
    out <- survey::svyglm(frm, design = design, family = quasibinomial) # model
    class(out) <- c("glm")

  }else if(model == "cox"){
    out <- survey::svycoxph(frm, design = design)
    class(out) <- c("coxph")
  }
  out
}

?survey::svycoxph
