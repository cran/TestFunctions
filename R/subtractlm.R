#' Subtract linear model from a function
#'
#' This returns a new function which a linear model has an r-squared of 0.
#'
#' @param func A function
#' @param d Number of input dimensions
#' @param n Number of points to use for the linear model
#'
#' @return A new function
#' @export
#'
#' @examples
#' subtractlm(ackley, 2)
#'
#' \donttest{
#'   f <- function(x) {
#'     if (is.matrix(x)) x[,1]^2
#'     else x[1]^2
#'   }
#'   ContourFunctions::cf(f)
#'   ContourFunctions::cf(subtractlm(f, 2), batchmax=Inf)
#' }
subtractlm <- function(func, d, n=d*100) {
  # x1 <- lhs::randomLHS(n, d)
  x1 <- random_LHS(n, d)
  y <- apply(x1, 1, func)
  df <- data.frame(x=x1, y=y)
  mod_lm <- lm(y ~ ., df)

  function(x, ...) {
    func(x, ...) - predict(mod_lm, data.frame(x=if (is.matrix(x)) {x} else {t(x)}))
  }
}

if (F) {
  curve(subtractlm(. %>% .^2, 1)(x))
  funcprofile(subtractlm(. %>% .^2, 1), 1)
}

