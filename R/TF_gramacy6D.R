#' gramacy6D: gramacy6D function
#' 6 dimensional function.
#' From Gramacy and Lee (2009).
#' @export
#' @references Gramacy, Robert B., and Herbert KH Lee.
#' "Adaptive design and analysis of supercomputer experiments."
#' Technometrics 51.2 (2009): 130-145.
#' @rdname test_func_apply
#' @examples
#' gramacy6D(runif(6))
#' gramacy6D(matrix(runif(6*20),ncol=6))
gramacy6D <- function(x, scale_it=T, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_gramacy6D, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_gramacy6D: gramacy6D function for evaluating a single point.
#'
#' From Gramacy and Lee (2009).
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#' @references Gramacy, Robert B., and Herbert KH Lee.
#' "Adaptive design and analysis of supercomputer experiments."
#' Technometrics 51.2 (2009): 130-145.
#'
#' @examples
#' TF_gramacy6D(rep(0,6))
#' TF_gramacy6D(rep(1,6))
TF_gramacy6D <- function(x) {
  exp(sin((.9*(x[1]+.48))^10)) + x[2]*x[3] + x[4]
}
