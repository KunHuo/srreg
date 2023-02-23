guess_model <- function(data, outcome = NULL, time = NULL, method = NULL){

  if(is.null(method)){
    vec.outcome <- data[[outcome]]
    vec.length  <- length(unique(vec.outcome))

    if(is.null(time)){
      if(is.factor(vec.outcome)){
        if(vec.length == 2L){
          method <- "logit"
        }else{
          stop("There is no executable method.", call. = FALSE)
        }
      }else if(is.numeric(vec.outcome)){
        if(vec.length == 2L){
          method <- "logit"
        }else{
          method <- "linear"
        }
      }else if(is.character(vec.outcome)){
        if(vec.length == 2L){
          method <- "logit"
        }else{
          stop("There is no executable method.", call. = FALSE)
        }
      }
    }else{
      method <- "cox"
    }
  }
  method
}
