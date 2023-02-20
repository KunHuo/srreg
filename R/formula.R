create_formula <- function(dependent, independents = NULL){
  if(length(dependent) == 2L){
    dependent <- sprintf("survival::Surv (%s, %s)", dependent[1], dependent[2]) # time, status
  }
  if(length(independents) == 1L){
    frm <- paste(dependent, independents, sep = " ~ ")
    frm <- stats::as.formula(frm)
  }else{
    frm <- paste(independents, collapse = " + ")
    frm <- paste(dependent, frm, sep = " ~ ")
    frm <- stats::as.formula(frm)
  }
  frm
}

create_formula2 <- function(outcome, exposure, strata, adjusted = NULL){
  if(length(outcome) == 2L){
    outcome <- sprintf("survival::Surv (%s, %s)", outcome[1], outcome[2])
  }
  frm <- paste(exposure, strata, sep = " * ")
  if(!is.null(adjusted)){
    adjusted <- paste(adjusted, collapse = " + ")
    frm <- paste(frm, adjusted, sep = " + ")
  }
  frm <- paste(outcome, frm, sep = " ~ ")
  frm <- stats::as.formula(frm)
  frm
}
