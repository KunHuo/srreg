matchps <- function(data,
                    outcome = NULL,
                    time =NULL,
                    exposure = NULL,
                    covariates = NULL,
                    positive = "auto",
                    method = "nearest",
                    distance = "glm",
                    link = "logit",
                    caliper = 0.2,
                    std.caliper = TRUE,
                    ratio = 1,
                    ...){

  outcome    <- srmisc::select_variable(data, outcome)
  time       <- srmisc::select_variable(data, time)
  exposure   <- srmisc::select_variable(data, exposure)
  covariates <- srmisc::select_variable(data, covariates)
  covariates <- setdiff(covariates, outcome)
  covariates <- setdiff(covariates, exposure)
  covariates <- setdiff(covariates, time)

  frm <- create_formula(exposure, covariates)
  mdata <- positive_event(data = data, outcome = exposure)

  fit <- MatchIt::matchit(formula = frm,
                   data = mdata,
                   method = method,
                   distance = distance,
                   link = link,
                   caliper = caliper,
                   std.caliper = std.caliper,
                   ratio = ratio,
                   ...)

  out <- list(fit = fit,
       data = data,
       outcome = outcome,
       time =time,
       exposure = exposure,
       covariates = covariates,
       positive = positive,
       method = method,
       distance = distance,
       link = link,
       caliper = caliper,
       std.caliper = std.caliper,
       ratio = ratio)

  class(out) <- c("matchps", "list")
  out
}


