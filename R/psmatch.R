#' Matching for causal inference
#'
#' @param data a data frame.
#' @param outcome outcome variable name.
#' @param time time variable name, for Cox regression.
#' @param exposure exposure variable name.
#' @param covariates covariate names, a vector or a list.
#' @param positive in which positive of outcome variable to make the comparison.
#' By default, positive is automatically defined. If outcome is a factor variable,
#' then positive is defined as the highest level. If outcome is a numerical
#' variable, then positive is defined as the largest value.
#' @param method the matching method to be used. Detail see [MatchIt::matchit()].
#' @param distance the distance measure to be used. [MatchIt::matchit()].
#' @param link when distance is specified as a string, an additional argument
#' controlling the link function used in estimating the distance measure.
#' @param caliper for methods that allow it, the width(s) of the caliper(s) to
#' use in matching. Should be a numeric vector with each value named according
#' to the variable to which the caliper applies. To apply to the distance measure,
#' the value should be unnamed. See the individual methods pages for information
#' on whether and how this argument is used. The default is NULL for no caliper.
#' @param std.caliper logical; when a caliper is specified, whether the the
#' caliper is in standard deviation units (TRUE) or raw units (FALSE).
#' Can either be of length 1, applying to all calipers, or of length equal
#' to the length of caliper. Default is TRUE.
#' @param ratio for methods that allow it, how many control units should be
#' matched to each treated unit in k:1 matching. Should be a single integer
#' value. See the individual methods pages for information on whether and how
#' this argument is used. The default is 1 for 1:1 matching.
#' @param ... additional arguments passed to the [MatchIt::matchit()].
#'
#' @return a list.
#'
#' @export
psm <- function(data,
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

  class(out) <- c("psmatch", "list")
  out
}


