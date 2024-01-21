# Just another file with functions

#' logistic: logistic function
#' 1 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' curve(logistic, from=-5,to=5)
#' curve(logistic(x,offset=.5, scl=15))
#' logistic(matrix(runif(20),ncol=1))
logistic <- function(x, scale_it=T, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_logistic, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_logistic: logistic function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#' @param offset Amount it should be offset
#' @param scl Scale parameter
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_logistic(0)
#' TF_logistic(1)
TF_logistic <- function(x, offset=0, scl=1) {
  1 / (1 + exp(-scl*(x-offset)))
}

#' logistic15: logistic15 function.
#' Same as logistic() except adjusted to be reasonable from 0 to 1,
#' has a center at 0.5 and scale of 15.
#' 1 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' curve(logistic15)
#' curve(logistic15(x,offset=.25))
#' logistic15(matrix(runif(20),ncol=1))
logistic15 <- function(x, scale_it=T, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_logistic15, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_logistic15: logistic15 function for evaluating a single point.
#' Same as logistic except adjusted to be reasonable from 0 to 1.
#'
#' @param x Input vector at which to evaluate.
#' @param offset Amount it should be offset
#' @param scl Scale parameter
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_logistic15(0)
#' TF_logistic15(1)
#' curve(Vectorize(TF_logistic15)(x))
TF_logistic15 <- function(x, offset=.5, scl=15) {
  1 / (1 + exp(-scl*(x-offset)))
}

#' logistic_plateau: logistic_plateau function. Sum of two logistics with
#' a plateau in the middle.
#' 1 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' curve(logistic_plateau(matrix(x,ncol=1)))
#' logistic_plateau(matrix(runif(20),ncol=1))
logistic_plateau <- function(x, scale_it=T, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_logistic_plateau, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_logistic_plateau: logistic_plateau function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_logistic_plateau(0)
#' TF_logistic_plateau(.5)
TF_logistic_plateau <- function(x) {
  logistic(x, offset=.15, scl=15) - logistic(x, offset=.85,scl=15)
}


#' vertigrad: vertigrad function
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' vertigrad(runif(2))
#' vertigrad(matrix(runif(2*20),ncol=2))
vertigrad <- function(x, scale_it=T, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_vertigrad, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_vertigrad: vertigrad function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_vertigrad(rep(0,2))
#' TF_vertigrad(rep(1,2))
TF_vertigrad <- function(x) {
  sin(2*pi*x[1]) + .5*sin(4*pi*x[1]) + x[2]^2
}

#' vertigrad_grad: gradient of the vertigrad function
#' 2 dimensional function.
#' 2 dimensional output.
#' @export
#' @rdname test_func_apply
#' @examples
#' vertigrad_grad(runif(2))
#' vertigrad_grad(matrix(runif(2*20),ncol=2))
vertigrad_grad <- function(x, scale_it=T, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_vertigrad_grad, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_vertigrad_grad: vertigrad_grad function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#' @references Forrester, A., & Keane, A. (2008). Engineering design via surrogate modelling: a practical guide. John Wiley & Sons.
#'
#' @examples
#' TF_vertigrad_grad(rep(0,2))
#' TF_vertigrad_grad(rep(1,2))
TF_vertigrad_grad <- function(x) {
  c(2*pi*cos(2*pi*x[1]) + .5*4*pi*cos(4*pi*x[1]), 2*x[2])
}


#' beambending: beambending function
#' 3 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' beambending(runif(3))
#' beambending(matrix(runif(3*20),ncol=3))
beambending <- function(x, scale_it=T, scale_low = c(10,1,0.1), scale_high = c(20,2,0.2), noise=0, ...) {
  test_func_apply(func=TF_beambending, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_beambending: beambending function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_beambending(rep(0,3))
#' TF_beambending(rep(1,3))
TF_beambending <- function(x) {
  # 4e-9 * L^3 / (b * h^3)
  # 4e-9 * x[1]^3 / (x[2] * x[3]^3)
  # Don't want to be on too small a scale
  4e-6 * x[1]^3 / (x[2] * x[3]^3)
}

#' chengsandu: chengsandu function
#' 2 dimensional function.
#' @export
#' @references Cheng, Haiyan, and Adrian Sandu. "Collocation least-squares polynomial chaos method." In Proceedings of the 2010 Spring Simulation Multiconference, p. 80. Society for Computer Simulation International, 2010.
#' @rdname test_func_apply
#' @examples
#' chengsandu(runif(2))
#' chengsandu(matrix(runif(2*20),ncol=2))
chengsandu <- function(x, scale_it=T, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_chengsandu, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_chengsandu: chengsandu function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#' @references Cheng, Haiyan, and Adrian Sandu. "Collocation least-squares polynomial chaos method." In Proceedings of the 2010 Spring Simulation Multiconference, p. 80. Society for Computer Simulation International, 2010.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_chengsandu(rep(0,2))
#' TF_chengsandu(rep(1,2))
TF_chengsandu <- function(x) {
  cos(x[1]+x[2]) * exp(x[1] * x[2])
}

#' steelcolumnstress: steelcolumnstress function. This is not checked and doesn't seem right.
#' The parameter L is not defined in paper. The ranges for variables are on different
#' distributions, not all uniform.
#' Don't use this.
#' 8 dimensional function.
#' @export
#' @references Kuschel, Norbert, and Rudiger Rackwitz. "Two basic problems in reliability-based structural optimization." Mathematical Methods of Operations Research 46, no. 3 (1997): 309-333.
#' @references Prikhodko, Pavel, and Nikita Kotlyarov. "Calibration of Sobol indices estimates in case of noisy output." arXiv preprint arXiv:1804.00766 (2018).
#' @rdname test_func_apply
#' @examples
#' steelcolumnstress(runif(8))
#' steelcolumnstress(matrix(runif(8*20),ncol=8))
steelcolumnstress <- function(x, scale_it=T,
                              scale_low = c(330,4e5,4.2e5,4.2e5,200,10,100,10,12600),
                              scale_high = c(470,6e5,7.8e5,7.8e5,400,30,500,50,29400), noise=0, ...) {
  test_func_apply(func=TF_steelcolumnstress, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_steelcolumnstress: steelcolumnstress function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#' @references Kuschel, Norbert, and Rudiger Rackwitz. "Two basic problems in reliability-based structural optimization." Mathematical Methods of Operations Research 46, no. 3 (1997): 309-333.
#' @references Prikhodko, Pavel, and Nikita Kotlyarov. "Calibration of Sobol indices estimates in case of noisy output." arXiv preprint arXiv:1804.00766 (2018).
#' @examples
#' TF_steelcolumnstress(rep(0,8))
#' TF_steelcolumnstress(rep(1,8))
TF_steelcolumnstress <- function(x) {
  # L isn't explained in papers??? Picking arbitrary number.
  L <- 1
  P <- x[1] + x[2] + x[3]
  Eb <- pi ^ 2 * x[8] * x[4] * x[5] * x[6] ^ 2 / 2 / L ^ 2
  t1 <- 1 / (2 * x[4] * x[5])
  t2 <- x[7] * Eb / (x[4] * x[5] * x[6] * (Eb - P))
  G <- x[1] - P * (t1 + t2)
  G
}

#' winkel: winkel function
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @references Winkel, Munir A., Jonathan W. Stallings, Curt B. Storlie, and
#' Brian J. Reich. "Sequential Optimization in Locally Important Dimensions."
#' arXiv preprint arXiv:1804.10671 (2018).
#' @examples
#' winkel(runif(2))
#' winkel(matrix(runif(2*20),ncol=2))
winkel <- function(x, scale_it=T, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_winkel, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_winkel: winkel function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#' @importFrom stats pnorm
#' @return Function output evaluated at x.
#' @export
#' @references Winkel, Munir A., Jonathan W. Stallings, Curt B. Storlie, and
#' Brian J. Reich. "Sequential Optimization in Locally Important Dimensions."
#' arXiv preprint arXiv:1804.10671 (2018).
#' @examples
#' TF_winkel(rep(0,2))
#' TF_winkel(rep(1,2))
TF_winkel <- function(x) {
  4*x[2]^2*pnorm(10*x[1]-4) + sin(5*pi*(x[1]-x[2]))*pnorm(4-10*x[1])
}
