
#' boreholeMV: A function based on the borehole function but with multivariate
#' output. Based on Worley's borehole function, but created
#' by Dr. Matthew Plumlee.
#' 8 dimensional function. 51 dimensional output.
#' @references Morris, M. D., Mitchell, T. J., & Ylvisaker, D. (1993). Bayesian design and analysis of computer experiments: use of derivatives in surface prediction. Technometrics, 35(3), 243-255.
#' @references Worley, Brian A. Deterministic uncertainty analysis. No. ORNL-6428. Oak Ridge National Lab., TN (USA), 1987.
#' @export
#' @rdname test_func_apply
#' @param NOD number of output dimensions
#' @examples
#' boreholeMV(runif(8))
#' boreholeMV(matrix(runif(80), ncol=8))
boreholeMV <- function(x, NOD=51, scale_it=T,
                     scale_low = c(.05,100,63070,990,63.1,700,1120,9855),
                     scale_high = c(.15,50000,115600,1110,116,820,1680,12045), noise=0) {
  test_func_applyMO(func=TF_boreholeMV, x=x, NOD=NOD, numoutdim=NOD, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}
#' TF_boreholeMV: A function taking in a single vector.
#' 8 dimensional function.
#' See corresponding function with "TF_" for more details.
#' @references Morris, M. D., Mitchell, T. J., & Ylvisaker, D. (1993). Bayesian design and analysis of computer experiments: use of derivatives in surface prediction. Technometrics, 35(3), 243-255.
#' @references Worley, Brian A. Deterministic uncertainty analysis. No. ORNL-6428. Oak Ridge National Lab., TN (USA), 1987.
#' @export
#' @param NOD number of output dimensions.
#' @rdname TF_OTL_Circuit
#' @examples
#' TF_boreholeMV(runif(8))
TF_boreholeMV <- function(x, NOD=51) {
  # 8 dim
  rw <- x[1]
  r <-  x[2]
  Tu <- x[3]
  Hu <- x[4]
  Tl <- x[5]
  Hl <- x[6]
  L <-  x[7]
  Kw <- x[8]

  m1 <- 2 * pi * Tu * (Hu - Hl)
  m2 <- log(r / rw)
  m3 <- 1 + 2 * L * Tu / (m2 * rw ^ 2 * Kw) + Tu / Tl

  Tot_v = abs(m1 / m2 / m3)
  # G1=(Tot_v)^matrix(seq(0.5,1.5,0.02),nrow=dim(x)[1],ncol=51,byrow=TRUE)
  G1=(Tot_v)^seq(0.5,1.5,l=NOD)
  G = cos(G1/400)*G1
  return(G)

}
