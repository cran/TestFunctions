#' Create function calculating the numerical gradient
#'
#' @param func Function to get gradient of.
#' @param ... Arguments passed to numDeriv::grad().
#'
#' @return A gradient function
#' @export
#'
#' @examples
#' numGrad(sin)
numGrad <- function(func, ...) {
  function(x) {numDeriv::grad(func=func, x=x, ...)}
}

#' Create function calculating the numerical hessian
#'
#' @param func Function to get hessian of
#' @param ... Arguments passed to numDeriv::hessian().
#'
#' @return A hessian function
#' @export
#'
#' @examples
#' numHessian(sin)
numHessian <- function(func, ...) {
  function(x) {numDeriv::hessian(func=func, x=x, ...)}
}
