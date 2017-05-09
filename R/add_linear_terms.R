#' add_linear_terms: Add linear terms to another function.
#' Allows you to easily change an existing function to include linear terms.
#'
#' @param func Function to add linear terms to
#' @param coeffs Linear coefficients, should have same length as function has dimensions
#'
#' @return Function with added linear terms
#' @export
#'
#' @examples
#' banana(c(.1,.2))
#' add_linear_terms(banana, coeffs=c(10,1000))(c(.1,.2))
add_linear_terms <- function(func, coeffs) {
  function(X, ...) {
    fout <- func(X)
    if (is.matrix(X)) {
      return(fout + c(coeffs %*% X))
    } else {
      return(fout + sum(coeffs * X))
    }
  }
}
