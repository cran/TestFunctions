% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subtractlm.R
\name{subtractlm}
\alias{subtractlm}
\title{Subtract linear model from a function}
\usage{
subtractlm(func, d, n = d * 100)
}
\arguments{
\item{func}{A function}

\item{d}{Number of input dimensions}

\item{n}{Number of points to use for the linear model}
}
\value{
A new function
}
\description{
This returns a new function which a linear model has an r-squared of 0.
}
\examples{
subtractlm(ackley, 2)

\donttest{
  f <- function(x) {
    if (is.matrix(x)) x[,1]^2
    else x[1]^2
  }
  ContourFunctions::cf(f)
  ContourFunctions::cf(subtractlm(f, 2), batchmax=Inf)
}
}
