#' General function for evaluating a test function
#'
#' @param func A function to evaluate
#' @param x Input value, either a matrix whose rows are points or
#' a vector for a single point. Be careful with 1-D functions.
#' @param scale_it Should the data be scaled from [0, 1]^D to
#' [scale_low, scale_high]? This means the input data is confined
#' to be in [0, 1]^D, but the function isn't.
#' @param scale_low Lower bound for each variable
#' @param scale_high Upper bound for each variable
#' @param noise If white noise should be added, specify the
#' standard deviation for normal noise
#' @param ... Additional parameters for func
#'
#' @return Function values at x
#' @importFrom stats rnorm rexp runif
#' @export
#'
#' @examples
#' x <- matrix(seq(0,1,length.out=10), ncol=1)
#' y <- test_func_apply(sin, x, TRUE, 0, 2*pi, .05)
#' plot(x,y)
#' curve(sin(2*pi*x), col=2, add=TRUE)
test_func_apply <- function(func, x, scale_it, scale_low, scale_high, noise=0, ...) {#browser()
  if (noise < 0 | !is.numeric(noise)) noise <- 0
  if (is.matrix(x)) {
    noise.out <- rnorm(nrow(x), 0, noise)
    #apply(x, 1, test_func_apply, func=func, scale_it=scale_it, scale_low=scale_low, scale_high=scale_high, ...)
    if (scale_it) {
      return(apply(x, 1, function(y, ...){func(y * (scale_high - scale_low) + scale_low, ...)}, ...) + noise.out)
    } else {
      return((apply(x, 1, func, ...)) + noise.out)
    }
  }
  # otherwise is single value
  noise.out <- rnorm(1, 0, noise)
  if (scale_it) {
    #return(func((x - scale_low) / (scale_high - scale_low)))
    return(func(x * (scale_high - scale_low) + scale_low, ...) + noise.out)
  }
  func(x, ...) + noise.out
}

#' Create a standard test function.
#'
#' This makes it easier to create
#' many functions that follow the same template.
#' R CMD check doesn't like the ... if this command is used to
#' create functions in the package, so it is not currently used.
#'
#' @param func A function that takes a vector representing a single point.
#' @param scale_it_ Should the function scale the inputs from [0, 1]^D to
#' [scale_low_, scale_high_] by default? This can be overridden when
#' actually giving the output function points to evaluate.
#' @param scale_low_ What is the default lower bound of the data?
#' @param scale_high_ What is the default upper bound of the data?
#' @param noise_ Should noise be added to the function by default?
#' @param ... Parameters passed to func when evaluating points.
#'
#' @return A test function created using the standard_test_func template.
#' @export
#'
#' @examples
#' .gaussian1 <- function(x, center=.5, s2=.01) {
#'   exp(-sum((x-center)^2/2/s2))
#' }
#' gaussian1 <- standard_test_func(.gaussian1, scale_it=FALSE, scale_low = c(0,0), scale_high = c(1,1))
#' curve(gaussian1(matrix(x,ncol=1)))
standard_test_func <- function(func, scale_it_=F, scale_low_ = NULL, scale_high_ = NULL, noise_=0, ...) {
  function(x, scale_it=scale_it_, scale_low = scale_low_, scale_high = scale_high_, noise=noise_) {
    test_func_apply(func=func, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
  }
}

