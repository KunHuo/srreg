#' Variance inflation factors
#'
#' @description
#' Calculates variance-inflation and generalized variance-inflation factors
#' (VIFs and GVIFs) for linear, generalized linear, and other regression models.
#'
#' @param fit a regression model.
#' @param ... not used.
#'
#' @return a data frame.
#' @export
vif <-function(fit, ...) {
  if (any(is.na(stats::coef(fit)))){
    stop("there are aliased coefficients in the model")
  }

  v <- stats::vcov(fit)
  assign <- attr(stats::model.matrix(fit), "assign")
  if (names(stats::coefficients(fit)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  }else {
    warning("No intercept: vifs may not be sensible.")
  }
  terms <- labels(terms(fit))
  n.terms <- length(terms)

  if (n.terms < 2){
    stop("model contains fewer than 2 terms")
  }

  R <- stats::cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")

  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs]))/detR
    result[term, 2] <- length(subs)
  }

  if (all(result[, 2] == 1)){
    result <- result[, 1]
  }else{
    result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  }
  result <- rownames_to_column(result)
  result
}
