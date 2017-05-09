#' add_null_dims: Add null dimensions to another function.
#' Allows you to pass in input data with any number of dimensions and it will only keep the first nactive.
#'
#' @param func Function to add null dimensions to
#' @param nactive Number of active dimensions in func
#'
#' @return Function that can take any dimensional input
#' @export
#'
#' @examples
#' banana(c(.1,.2))
#' # banana(c(.1,.2,.4,.5,.6,.7,.8)) # gives warning
#' add_null_dims(banana, nact=2)(c(.1,.2,.4,.5,.6,.7,.8))
add_null_dims <- function(func, nactive) {
  function(X, ...) {
    if (is.matrix(X)) {
      return(func(X[,1:nactive]))
    } else {
      return(func(X[1:nactive]))
    }
  }
}
