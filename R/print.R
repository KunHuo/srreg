#' Print srreg
#'
#' @param x a srreg object
#' @param ... unused.
#'
#' @keywords internal
#' @export
print.srreg <- function(x, ...){
  if("P value" %in% names(x)){
    x[["P value"]] <- srmisc::fmt_signif(x[["P value"]])
  }

  STAT.NAME <- c("t value", "Std. error", "Wald", "B", "\u03b2")

  if(any(STAT.NAME %in% names(x))){
    STAT.NAME <- STAT.NAME[STAT.NAME %in% names(x)]

    x[STAT.NAME] <- lapply(x[STAT.NAME], function(i){
      ifelse(is.na(i), NA, format(i, justify = "right"))
    })
  }
  srmisc::print_booktabs(x, adj = c("l", "c"), ...)
}
