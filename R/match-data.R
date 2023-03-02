#' Construct a matched dataset from a matchps object
#'
#' @param object a matchps object; the output of a call to [matchps()].
#' @param type type matched dataset.
#' @param drop.unmatched logical; whether the returned data frame should contain
#' all units (FALSE) or only units that were matched (i.e., have a matching
#' weight greater than zero) (TRUE). Default is TRUE to drop unmatched units.
#' @param ... unused.
#'
#' @return a data frame.
#' @export
match_data <- function(object, type = c("psm", "iptw", "unmatched"), drop.unmatched = TRUE, ...){
  type <- match.arg(type)
  if(type == "psm"){
    MatchIt::match.data(object$fit, data = object$data, drop.unmatched = drop.unmatched)
  }else if(type == "iptw"){
    dat <- MatchIt::match.data(object$fit, data = object$data, drop.unmatched = FALSE)
    if(is.null(object$control)){
      levels <- levels(dat[[object$exposure]])
      pt <- sum(dat[[object$exposure]] == levels[2]) / nrow(dat)
      wt1 <- pt / dat$distance
      wt0 <- (1-pt) / (1 - dat$distance)
      dat$iptw <- ifelse(dat[[object$exposure]] == levels[1], wt0, wt1)
    }else{
      pt <- sum(dat[[object$exposure]] != object$control) / nrow(dat)
      wt1 <- pt / dat$distance
      wt0 <- (1-pt) / (1 - dat$distance)
      dat$iptw <- ifelse(dat[[object$exposure]] == object$control, wt0, wt1)
    }
    dat$distance <- NULL
    dat$weights <- NULL
    dat$subclass <- NULL
    dat
  }else{
    object$data
  }
}
