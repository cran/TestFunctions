% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions2.R
\name{TF_steelcolumnstress}
\alias{TF_steelcolumnstress}
\title{TF_steelcolumnstress: steelcolumnstress function for evaluating a single point.}
\usage{
TF_steelcolumnstress(x)
}
\arguments{
\item{x}{Input vector at which to evaluate.}
}
\value{
Function output evaluated at x.
}
\description{
TF_steelcolumnstress: steelcolumnstress function for evaluating a single point.
}
\examples{
TF_steelcolumnstress(rep(0,8))
TF_steelcolumnstress(rep(1,8))
}
\references{
Kuschel, Norbert, and Rudiger Rackwitz. "Two basic problems in reliability-based structural optimization." Mathematical Methods of Operations Research 46, no. 3 (1997): 309-333.

Prikhodko, Pavel, and Nikita Kotlyarov. "Calibration of Sobol indices estimates in case of noisy output." arXiv preprint arXiv:1804.00766 (2018).
}
