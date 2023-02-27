auto_model <- function(data,
                        outcome = NULL,
                        time = NULL,
                        model = c("auto", "linear", "logit", "cox", "poson", "logbinom", "multinom")){

  model <- match.arg(model)

  if(model == "auto"){
    vec.outcome <- data[[outcome]]
    vec.length  <- length(unique(vec.outcome))

    if(is.null(time)){
      if(is.factor(vec.outcome)){
        if(vec.length == 2L){
          model <- "logit"
        }else{
          model <- ifelse(is.ordered(vec.outcome), "", "multinom")
        }
      }else if(is.numeric(vec.outcome)){
        if(vec.length == 2L){
          model <- "logit"
        }else{
          model <- "linear"
        }
      }else if(is.character(vec.outcome)){
        if(vec.length == 2L){
          model <- "logit"
        }else{
          model <- "multinom"
        }
      }
    }else{
      model <- "cox"
    }
  }
  model
}


model_names <- function(model){
  switch(model,
         linear   = "multiple linear regression",
         logit    = "binary logistc regression",
         cox      = "Cox proportional hazards regression",
         poisson  = "",
         logbinom = "",
         multinom = "")
}



LRT <- function(data, outcome, time, exposure, covariates, strata, positive, model, args = list(), digits.pvalue = 3){

  model <- auto_model(data, outcome = outcome, time = time, model = model)

  output <- lapply(strata, \(svar){
    covariates <- setdiff(covariates, svar)
    if(srmisc::is_empty(covariates)){
      covariates <- NULL
    }

    frm1 <- create_formula(c(time, outcome), c(exposure, svar, covariates))
    frm2 <- create_formula2(c(time, outcome), exposure, svar, covariates)

   if(model == "linear"){
     fit1 <- srmisc::do_call(model, data = data, formula = frm1, args)
     fit2 <- srmisc::do_call(model, data = data, formula = frm2, args)
   }else{
     fit1 <- srmisc::do_call(model, data = data, formula = frm1, positive = positive, args)
     fit2 <- srmisc::do_call(model, data = data, formula = frm2, positive = positive, args)
   }

    pvalue <- stats::anova(fit1, fit2, test = "LRT")
    pvalue <- srmisc::fmt_pvalue(pvalue[, ncol(pvalue)][2], digits = digits.pvalue)
    res <- data.frame(term = svar, pvalue)
    names(res)[2] <- "P for interaction"
    res
  })
  do.call(rbind, output)
}


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

positive_event <- function(data, outcome, positive = "auto"){

  if(is.null(positive)){
    data
  }else{
    if(positive == "auto"){
      if(is.numeric(data[[outcome]])){
        positive <- max(data[[outcome]])
      }else if(is.factor(data[[outcome]])){
        positive <- levels(data[[outcome]])[2]
      }else{
        stop("You need to specify the positive event of outcome.", call. = FALSE)
      }
    }
    data[[outcome]] <-  ifelse(data[[outcome]] == positive, 1, 0)
    data
  }
}

