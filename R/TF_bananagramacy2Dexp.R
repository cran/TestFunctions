#' bananagramacy2Dexp: bananagramacy2Dexp function
#' 6 dimensional function.
#' First two dimensions are banana function,
#' next two are the gramacy2Dexp function,
#' last two are null dimensions
#' @export
#' @rdname test_func_apply
#' @examples
#' bananagramacy2Dexp(runif(6))
#' bananagramacy2Dexp(matrix(runif(6*20),ncol=6))
bananagramacy2Dexp <- function(x, scale_it=T, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_bananagramacy2Dexp, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_bananagramacy2Dexp: bananagramacy2Dexp function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_bananagramacy2Dexp(rep(0,6))
#' TF_bananagramacy2Dexp(rep(1,6))
TF_bananagramacy2Dexp <- function(x) {
  x1 <- c(x[1]*40-20, x[2]*15-10)
  x2 <- x[3:4]*8-2
  t1 <- exp(-.005*x1[1]^2-.5*(x1[2]+.03*x1[1]^2-3)^2)
  t2 <- x2[1] * exp(-sum(x2^2))
  t1 + t2
}


#' bananatimesgramacy2Dexp: bananatimesgramacy2Dexp function
#' 6 dimensional function.
#' First two dimensions are banana function,
#' next two are the gramacy2Dexp function,
#' last two are null dimensions
#' @export
#' @rdname test_func_apply
#' @examples
#' bananatimesgramacy2Dexp(runif(6))
#' bananatimesgramacy2Dexp(matrix(runif(6*20),ncol=6))
bananatimesgramacy2Dexp <- function(x, scale_it=T, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_bananatimesgramacy2Dexp, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_bananatimesgramacy2Dexp: bananatimesgramacy2Dexp function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_bananatimesgramacy2Dexp(rep(0,6))
#' TF_bananatimesgramacy2Dexp(rep(1,6))
TF_bananatimesgramacy2Dexp <- function(x) {
  x1 <- c(x[1]*40-20, x[2]*15-10)
  x2 <- x[3:4]*8-2
  t1 <- exp(-.005*x1[1]^2-.5*(x1[2]+.03*x1[1]^2-3)^2)
  t2 <- x2[1] * exp(-sum(x2^2))
  t1 * t2
}
