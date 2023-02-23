taskreg <- function(data, outcome, time, exposure, covariates, positive, model = auto()){

  outcome  <- srmisc::select_variable(data, outcome)
  time     <- srmisc::select_variable(data, time)
  exposure <- srmisc::select_variable(data, exposure)

  list(data = data,
       outcome = outcome,
       time = time,
       exposure = exposure,
       model = model)

}
