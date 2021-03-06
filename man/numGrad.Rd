% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/derivatives.R
\name{numGrad}
\alias{numGrad}
\title{Create function calculating the numerical gradient}
\usage{
numGrad(func, ...)
}
\arguments{
\item{func}{Function to get gradient of.}

\item{...}{Arguments passed to numDeriv::grad().}
}
\value{
A gradient function
}
\description{
Create function calculating the numerical gradient
}
\examples{
numGrad(sin)
}
