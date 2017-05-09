#' add_noise: Adds noise to any function
#'
#' @param func Function to add noise to.
#' @param noise Standard deviation of Gaussian noise
#' @param noise_type Type of noise, only option now is "Gauss" for Gaussian noise.
#'
#' @return A function that has noise
#' @export
#'
#' @examples
#' tf <- add_noise(function(x)sin(2*x*pi));curve(tf)
#' tf <- add_noise(function(x)sin(2*x*pi), noise=.1);curve(tf)
add_noise <- function(func, noise=0, noise_type="Gauss") {
  function(...) {
    fout <- func(...)
    foutlen <- length(fout)
    if (noise_type == "Gauss") {
      noise <- rnorm(n=foutlen, mean=0, sd=noise)
    }
    fout + noise
  }
}
