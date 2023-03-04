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

  res.adj <- associate(data = object$data,
            outcome = outcome,
            time = time,
            exposure = exposure,
            positive = positive,
            covariates = covariates,
            ...)


  model <- auto_model(object$data, outcome, time, model)
  frm <- create_formula(c(time, outcome), c(exposure))


  data.psm <- psm_data(object)
  class(data.psm) <- "data.frame"
  if(model == "linear"){
    fit.psm <- srmisc::do_call(model,
                           data = data.psm,
                           formula = frm,
                           weights = data.psm$weights)
  }else{
    fit.psm <- srmisc::do_call(model,
                           data = data.psm,
                           formula = frm,
                           positive = positive,
                           weights = data.psm$weights)
  }
  res.psm <- lmtest::coeftest(fit.psm, vcov. = sandwich::vcovCL, cluster = data.psm$subclass)
  res.psm

  # data.iptw <- psm_data(object, type = "iptw")
  #
  # iptw_model(data = data.iptw, frm = frm, model = model)

}


iptw_model <- function(data, frm, model, ...){

   design <- survey::svydesign(ids = ~1, weights = data[["iptw"]], data = data)

  if(model == "linear"){
    out <- survey::svyglm(frm, design = design)
    class(out) <- c("lm")

  }else if(model == "logit"){
    out <- survey::svyglm(frm, design = design, family = quasibinomial)
    class(out) <- c("glm")

  }else if(model == "cox"){
    out <- survey::svycoxph(frm, design = design)
    class(out) <- c("coxph")
  }
  out
}
