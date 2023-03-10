% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poisson.R
\name{poson2}
\alias{poson2}
\title{Fitting poisson models}
\usage{
poson2(
  data,
  outcome = NULL,
  exposure = NULL,
  covariates = NULL,
  positive = "auto",
  ...
)
}
\arguments{
\item{data}{a data frame, list or environment (or object coercible by as.data.
frame to a data frame) containing the variables in the model.}

\item{outcome}{outcome variable name.}

\item{exposure}{exposure variable name.}

\item{covariates}{covariate names, a vector or a list.}

\item{positive}{in which positive of outcome variable to make the comparison.
By default, positive is automatically defined. If outcome is a factor variable,
then positive is defined as the highest level. If outcome is a numerical
variable, then positive is defined as the largest value.}

\item{...}{additional arguments to be passed to \code{\link[=glm]{glm()}} function.}
}
\value{
an object of class inheriting from "glm" which inherits from the
class "lm".
}
\description{
Fitting poisson models
}
