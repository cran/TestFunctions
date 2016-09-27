#' Test function.
#'
#' branin: A function.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' branin(runif(2))
#' branin(matrix(runif(20), ncol=2))
branin <- function(x, scale_it=T, scale_low = c(-5, 0), scale_high = c(10, 15)) {
  # 2 dim, http://www.sfu.ca/~ssurjano/branin.html
  test_func_apply(func=TF_branin, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high)
}
#' Base test function.
#'
#' TF_branin: A function taking in a single vector.
#' 2 dimensional function.
#' See corresponding function with "TF_" for more details.
#' @export
#' @rdname TF_OTL_Circuit
#' @param a Parameter for TF_branin
#' @param b Parameter for TF_branin
#' @param cc Parameter for TF_branin
#' @param r Parameter for TF_branin
#' @param s Parameter for TF_branin
#' @param tt Parameter for TF_branin
#' @examples
#' TF_branin(runif(2))
TF_branin <- function(x, a=1, b=5.1/(4*pi^2), cc=5/pi, r=6, s=10, tt=1/(8*pi)) {
  a * (x[2] - b * x[1]^2 + cc * x[1] - r)^2 + s * (1 - tt) * cos(x[1]) + s
}


#' borehole: A function estimating water flow through a borehole.
#' 8 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' borehole(runif(8))
#' borehole(matrix(runif(80), ncol=8))
borehole <- function(x, scale_it=T,
                     scale_low = c(.05,100,63070,990,63.1,700,1120,9855),
                     scale_high = c(.15,50000,115600,1110,116,820,1680,12045)) {
  test_func_apply(func=TF_borehole, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high)
}
#' TF_borehole: A function taking in a single vector.
#' 8 dimensional function.
#' See corresponding function with "TF_" for more details.
#' @export
#' @rdname TF_OTL_Circuit
#' @examples
#' TF_borehole(runif(8))
TF_borehole <- function(x) {
  # 8 dim, NOT uniform
  # See: http://www.sfu.ca/~ssurjano/borehole.html
  2 * pi * x[3] * (x[4] - x[6]) /
    (log(x[2] / x[1]) *
       (1 + 2 * x[7] * x[3] / log(x[2] / x[1]) * x[1]^2 * x[8]) +
       x[3] / x[5])
}


#' franke: A function.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' franke(runif(2))
franke <- function(x, scale_it=F, scale_low = c(0,0), scale_high = c(1,1)) {
  # 2 dim, http://www.sfu.ca/~ssurjano/franke2d.html
  test_func_apply(func=TF_franke, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high)
}
#' TF_franke: A function taking in a single vector.
#' 2 dimensional function.
#' See corresponding function with "TF_" for more details.
#' @export
#' @rdname TF_OTL_Circuit
#' @examples
#' TF_franke(runif(2))
TF_franke <- function(x) {
  0.75 * exp(-(9 * x[1] - 2)^2 / 4 - (9 * x[2] - 2)^2 / 4) +
    0.75 * exp(-(9 * x[1] + 1)^2 / 49 - (9 * x[2] + 1)^2 / 10) +
    0.5 * exp(-(9 * x[1] - 7)^2 / 4 - (9 * x[2] - 3)^2 / 4) +
    -0.2 * exp(-(9 * x[1] - 4)^2 - (9 * x[2] - 7)^2)
}


#' zhou1998: A function.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' zhou1998(runif(2))
zhou1998 <- function(x, scale_it=F, scale_low = c(0,0), scale_high = c(1,1)) {
  # 2 dim, http://www.sfu.ca/~ssurjano/branin.html
  test_func_apply(func=TF_zhou1998, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high)
}
#' TF_zhou1998: A function taking in a single vector.
#' 2 dimensional function.
#' See corresponding function with "TF_" for more details.
#' @export
#' @rdname TF_OTL_Circuit
#' @examples
#' TF_zhou1998(runif(2))
TF_zhou1998 <- function(x) {
  # Any dim, http://www.sfu.ca/~ssurjano/zhou98.html
  d <- length(x)
  phi1 <- (2 * pi)^(-d / 2) * exp(-.5 * sum((10 * (x - 1 / 3))^2))
  phi2 <- (2 * pi)^(-d / 2) * exp(-.5 * sum((10 * (x - 2 / 3))^2))
  10^d / 2 * (phi1 + phi2)
}


#' currin1991: A function.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' currin1991(runif(2))
currin1991 <- function(x, scale_it=F, scale_low = c(0,0), scale_high = c(1,1)) {
  # 2 dim, http://www.sfu.ca/~ssurjano/curretal91.html
  test_func_apply(func=TF_currin1991, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high)
}
#' TF_currin1991: A function taking in a single vector.
#' 2 dimensional function.
#' See corresponding function with "TF_" for more details.
#' @export
#' @rdname TF_OTL_Circuit
#' @examples
#' TF_currin1991(runif(2))
TF_currin1991 <- function(x) {
  4.9 + 21.15 * x[1] - 2.17 * x[2] - 15.88 * x[1]^2 -
    1.38 * x[2]^2 - 5.26 * x[1] * x[2]
}


#' lim2002: Some function?
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' lim2002(runif(2))
lim2002 <- function(x, scale_it=F, scale_low = c(0,0), scale_high = c(1,1)) {
  # 2 dim, http://www.sfu.ca/~ssurjano/limetal02pol.html
  test_func_apply(func=TF_lim2002, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high)
}
#' TF_lim2002: A function taking in a single vector.
#' 2 dimensional function.
#' See corresponding function with "TF_" for more details.
#' @export
#' @rdname TF_OTL_Circuit
#' @examples
#' TF_lim2002(runif(2))
TF_lim2002 <- function(x) {
  9 + 2.5 * x[1] - 17.5 * x[2] + 2.5 * x[1] * x[2] + 19 * x[2]^2 -
    7.5 * x[1]^3 - 2.5 * x[1] * x[2]^2 - 5.5 * x[2]^4 + x[1]^3 * x[2]^2
}


#' banana: A banana shaped function.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' banana(runif(2))
#' x <- y <- seq(0, 1, len=100)
#' z <- outer(x, y, Vectorize(function(a, b){banana(c(a, b))}))
#' contour(x, y, z)
banana <- function(x, scale_it=T, scale_low = c(-20,-10), scale_high = c(20,5)) {
  # 2 dim, See Roshan SMED
  test_func_apply(func=TF_banana, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high)
}
#' TF_banana: A function taking in a single vector.
#' 2 dimensional function.
#' See corresponding function with "TF_" for more details.
#' @export
#' @rdname TF_OTL_Circuit
#' @examples
#' TF_banana(runif(2))
TF_banana <- function(x){
  exp(-.005*x[1]^2-.5*(x[2]+.03*x[1]^2-3)^2)
}

#' gaussian1: A Gaussian function centered at 0.5.
#' Any dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' gaussian1(runif(2))
gaussian1 <- function(x, scale_it=F, scale_low = c(0, 0), scale_high = c(1,1)) {
  # 2 dim, http://www.sfu.ca/~ssurjano/branin.html
  test_func_apply(func=TF_gaussian1, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high)
}
#gaussian1 <- standard_test_func(TF_gaussian1, scale_it=F, scale_low = c(0,0), scale_high = c(1,1))

#' TF_gaussian1: A function taking in a single vector.
#' Any dimensional function.
#' See corresponding function with "TF_" for more details.
#' @param center Where to center the function, a vector.
#' @param s2 Variance of the Gaussian.
#' @export
#' @rdname TF_OTL_Circuit
#' @examples
#' TF_gaussian1(runif(2))
TF_gaussian1 <- function(x, center=.5, s2=.01) {
  exp(-sum((x-center)^2/2/s2))
}

#' sinumoid: A sinusoid added to a sigmoid function.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' sinumoid(runif(2))
#' x <- y <- seq(0, 1, len=100)
#' z <- outer(x, y, Vectorize(function(a, b){sinumoid(c(a, b))}))
#' contour(x, y, z)
sinumoid <- function(x, scale_it=F, scale_low = c(0, 0), scale_high = c(1,1)) {
  # 2 dim, http://www.sfu.ca/~ssurjano/branin.html
  test_func_apply(func=TF_sinumoid, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high)
}
#sinumoid <- standard_test_func(TF_sinumoid, scale_it=F, scale_low = c(0,0), scale_high = c(1,1), noise=0)
#' TF_sinumoid: A function taking in a single vector.
#' 2 dimensional function.
#' See corresponding function with "TF_" for more details.
#' @export
#' @rdname TF_OTL_Circuit
#' @examples
#' TF_sinumoid(runif(2))
TF_sinumoid <- function(x){
  sum(sin(2*pi*x*3)) + 20/(1+exp(-80*(x[[1]]-.5)))
}

#' waterfall: A sinusoid added to a sigmoid function.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' waterfall(runif(2))
waterfall <- sinumoid


#' sqrtsin: A square root of a sine function.
#' Any dimensional function.
#' @param freq Wave frequency for sqrtsin and powsin
#' @export
#' @rdname test_func_apply
#' @examples
#' sqrtsin(runif(1))
#' curve(sqrtsin(matrix(x,ncol=1)))
sqrtsin <- function(x, scale_it=F, scale_low = c(0,0), scale_high = c(1,1), freq=2*pi) {
  test_func_apply(func=TF_sqrtsin, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, freq=freq)
}
#' TF_sqrtsin: A function taking in a single vector.
#' Any dimensional function.
#' See corresponding function with "TF_" for more details.
#' @param freq Wave frequency for TF_sqrtsin and TF_powsin
#' @export
#' @rdname TF_OTL_Circuit
#' @examples
#' TF_sqrtsin(runif(2))
TF_sqrtsin <- function(x, freq=2*pi) {
  ss <- sum(sin(freq*x))
  sqrt(abs(ss))*sign(ss)
}


#' powsin: A sine function raised to a power keeping its original sign.
#' Any dimensional function.
#' @rdname test_func_apply
#' @param pow Power for powsin
#' @export
#'
#' @examples
#' powsin(runif(1))#,pow=2)
powsin <- function(x, scale_it=F, scale_low = c(0, 0), scale_high = c(1,1), noise=0, freq=2*pi, pow=.7) {
  test_func_apply(func=TF_powsin, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, freq=freq, pow=pow)
}
#powsin <- standard_test_func(TF_powsin, scale_it=F, scale_low = c(0,0), scale_high = c(1,1), pow=1)

#' TF_powsin: A function taking in a single vector.
#' Any dimensional function.
#' See corresponding function with "TF_" for more details.
#' @param pow Power to raise wave to for TF_powsin.
#' @export
#' @rdname TF_OTL_Circuit
#' @examples
#' TF_powsin(runif(2))
TF_powsin <- function(x, freq=2*pi, pow=.7) {
  ss <- sum(sin(freq*x))
  (abs(ss) ^ pow) * sign(ss)
}

#' OTL_Circuit: OTL Circuit.
#' 6 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' OTL_Circuit(runif(6))
#' OTL_Circuit(matrix(runif(60),ncol=6))
OTL_Circuit <- function(x, scale_it=T, scale_low = c(50,25,0.5,1.2,0.25,50), scale_high = c(150,70,3,2.5,1.2,300)) {
test_func_apply(func=TF_OTL_Circuit, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high)
}
#OTL_Circuit <- standard_test_func(TF_OTL_Circuit, scale_it=T,
#                                  scale_low = c(50,25,0.5,1.2,0.25,50),
#                                  scale_high = c(150,70,3,2.5,1.2,300))

#' TF_OTL_Circuit: OTL Circuit function for evaluating a single point
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_OTL_Circuit(c(50,25,0.5,1.2,0.25,50))
TF_OTL_Circuit <- function(x) {
  Vb1 <- 12*x[2] / (x[1] + x[2])
  BRc29 <- x[6] * (x[5] + 9) #+ x[3]
  t1 <- (Vb1 + 0.74) * BRc29 / (BRc29 + x[3])
  t2 <- 11.35 * x[3] / (BRc29 + x[3])
  t3 <- .74 * x[3] * BRc29 / ((BRc29 + x[3]) * x[4])
  t1 + t2 + t3
}
