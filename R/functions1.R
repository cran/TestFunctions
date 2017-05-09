#' Test function.
#'
#' branin: A function.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' branin(runif(2))
#' branin(matrix(runif(20), ncol=2))
branin <- function(x, scale_it=T, scale_low = c(-5, 0), scale_high = c(10, 15), noise=0) {
  # 2 dim, http://www.sfu.ca/~ssurjano/branin.html
  test_func_apply(func=TF_branin, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
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
                     scale_high = c(.15,50000,115600,1110,116,820,1680,12045), noise=0) {
  test_func_apply(func=TF_borehole, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
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
franke <- function(x, scale_it=F, scale_low = c(0,0), scale_high = c(1,1), noise=0) {
  # 2 dim, http://www.sfu.ca/~ssurjano/franke2d.html
  test_func_apply(func=TF_franke, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
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
zhou1998 <- function(x, scale_it=F, scale_low = c(0,0), scale_high = c(1,1), noise=0) {
  # 2 dim, http://www.sfu.ca/~ssurjano/branin.html
  test_func_apply(func=TF_zhou1998, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
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
currin1991 <- function(x, scale_it=F, scale_low = c(0,0), scale_high = c(1,1), noise=0) {
  # 2 dim, http://www.sfu.ca/~ssurjano/curretal91.html
  test_func_apply(func=TF_currin1991, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
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
lim2002 <- function(x, scale_it=F, scale_low = c(0,0), scale_high = c(1,1), noise=0) {
  # 2 dim, http://www.sfu.ca/~ssurjano/limetal02pol.html
  test_func_apply(func=TF_lim2002, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
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
banana <- function(x, scale_it=T, scale_low = c(-20,-10), scale_high = c(20,5), noise=0) {
  # 2 dim, See Roshan SMED
  test_func_apply(func=TF_banana, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
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
gaussian1 <- function(x, scale_it=F, scale_low = c(0, 0), scale_high = c(1,1), noise=0) {
  # 2 dim, http://www.sfu.ca/~ssurjano/branin.html
  test_func_apply(func=TF_gaussian1, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
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
sinumoid <- function(x, scale_it=F, scale_low = c(0, 0), scale_high = c(1,1), noise=0) {
  # 2 dim, http://www.sfu.ca/~ssurjano/branin.html
  test_func_apply(func=TF_sinumoid, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
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
sqrtsin <- function(x, scale_it=F, scale_low = c(0,0), scale_high = c(1,1), noise=0, freq=2*pi) {
  test_func_apply(func=TF_sqrtsin, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, freq=freq)
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
  test_func_apply(func=TF_powsin, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, freq=freq, pow=pow)
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
OTL_Circuit <- function(x, scale_it=T, scale_low = c(50,25,0.5,1.2,0.25,50), scale_high = c(150,70,3,2.5,1.2,300), noise=0) {
  test_func_apply(func=TF_OTL_Circuit, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
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



#' GoldsteinPrice: Goldstein-Price function. Exponential scale, you
#' might want to use GoldsteinPriceLog instead
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' GoldsteinPrice(runif(2))
#' GoldsteinPrice(matrix(runif(60),ncol=2))
GoldsteinPrice <- function(x, scale_it=T, scale_low = c(-2,-2), scale_high = c(2,2), noise=0) {
  test_func_apply(func=TF_GoldsteinPrice, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}

#' TF_GoldsteinPrice: Goldstein Price function for evaluating a single point
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_GoldsteinPrice(c(0, -1)) # minimum
TF_GoldsteinPrice <- function(x) {
  a1 <- 1 + (x[1]+x[2]+1)^2 * (19-14*x[1]+3*x[1]^2-14*x[2]+6*x[1]*x[2]+3*x[2]^2)
  a2 <- 30 + (2*x[1]-3*x[2])^2 * (18-32*x[1]+12*x[1]^2+48*x[2]-36*x[1]*x[2]+27*x[2]^2)
  a1 * a2
}



#' GoldsteinPriceLog: Goldstein-Price function on a log scale.
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' GoldsteinPriceLog(runif(2))
#' GoldsteinPriceLog(matrix(runif(60),ncol=2))
GoldsteinPriceLog <- function(x, scale_it=T, scale_low = c(-2,-2), scale_high = c(2,2), noise=0) {
  test_func_apply(func=TF_GoldsteinPriceLog, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}

#' TF_GoldsteinPrice: Goldstein Price function for evaluating a single point
#' on a log scale, normalized to have mean 0 and variance 1.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_GoldsteinPriceLog(c(0, -1)) # minimum
TF_GoldsteinPriceLog <- function(x) {
  a1 <- 1 + (x[1]+x[2]+1)^2 * (19-14*x[1]+3*x[1]^2-14*x[2]+6*x[1]*x[2]+3*x[2]^2)
  a2 <- 30 + (2*x[1]-3*x[2])^2 * (18-32*x[1]+12*x[1]^2+48*x[2]-36*x[1]*x[2]+27*x[2]^2)
  a3 <- a1 * a2
  (log(a3) - 8.693) / 2.427
}




#' ackley: Ackley function.
#' 2 dimensional function.
#' @param a A constant for ackley()
#' @param b A constant for ackley()
#' @param c A constant for ackley()
#' @export
#' @rdname test_func_apply
#' @examples
#' ackley(runif(2))
#' ackley(matrix(runif(60),ncol=2))
ackley <- function(x, scale_it=T, scale_low = -32.768, scale_high = 32.768, noise=0, a=20, b=0.2, c=2*pi) {
  test_func_apply(func=TF_ackley, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, a=a, b=b, c=c)
}

#' TF_ackley: Ackley function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @param a A constant for ackley()
#' @param b A constant for ackley()
#' @param c A constant for ackley()
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_ackley(c(0, 0)) # minimum of zero, hard to solve
TF_ackley <- function(x, a=20, b=0.2, c=2*pi) {
  -a * exp(-b*sqrt(mean(x^2))) - exp(mean(cos(c*x))) + a + exp(1)
}



#' piston: Piston simulation function.
#' 7 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' piston(runif(7))
#' piston(matrix(runif(7*20),ncol=7))
piston <- function(x, scale_it=T, scale_low = c(30,.005,.002,1e3,9e4,290,340), scale_high = c(60,.02,.01,5e3,11e4,296,360), noise=0) {
  test_func_apply(func=TF_piston, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}

#' TF_piston: Piston simulation function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_piston(c(30,.005,.002,1e3,9e4,290,340)) # minimum of zero, hard to solve
TF_piston <- function(x) {
  M <- x[1]
  S <- x[2]
  V0 <- x[3]
  k <- x[4]
  P0 <- x[5]
  Ta <- x[6]
  T0 <- x[7]
  A <- P0*S + 19.62*M -k*V0/S
  V <- S/(2*k) * (sqrt(A^2+4*k*P0*V0/T0*Ta) - A)
  C <- 2*pi * sqrt(M / (k + S^2*P0*V0/T0*Ta/V^2))
  C
}



#' wingweight: Wing weight function.
#' 10 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' wingweight(runif(10))
#' wingweight(matrix(runif(10*20),ncol=10))
wingweight <- function(x, scale_it=T, scale_low = c(150,220,6,-10,16,.5,.08,2.5,1700,.025), scale_high = c(200,300,10,10,45,1,.18,6,2500,.08), noise=0) {
  test_func_apply(func=TF_wingweight, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}

#' TF_wingweight: Wing weight function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_wingweight(c(150,220,6,-10,16,.5,.08,2.5,1700,.025)) # minimum of zero, hard to solve
TF_wingweight <- function(x) {
  Sw <- x[1]
  Wfw <- x[2]
  A <- x[3]
  Lambda <- x[4] * pi / 180 # convert degrees to radians
  q <- x[5]
  lambda <- x[6]
  tc <- x[7]
  Nz <- x[8]
  Wdg <- x[9]
  Wp <- x[10]
  0.036 * Sw^.758 * Wfw^.0035 * (A/cos(Lambda)^2)^.6 * q^.006 * lambda^.04 * (100*tc/cos(Lambda))^-.3 * (Nz*Wdg)^.49 + Sw*Wp
}




#' welch: Welch et al (1992) function.
#' 20 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' welch(runif(20))
#' welch(matrix(runif(20*20),ncol=20))
welch <- function(x, scale_it=T, scale_low = c(-.5), scale_high = c(.5), noise=0) {
  test_func_apply(func=TF_welch, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}

#' TF_welch: Welch function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_welch(rep(0,20)) # minimum of zero, hard to solve
TF_welch <- function(x) {
  5*x[12]/(1+x[1]) + 5*(x[4]-x[20])^2 + x[5] + 40*x[19]^3 +
    -5*x[19] + .05*x[2] + .08*x[3] - .03*x[6] +
    .03*x[7] - .09*x[9] - .01*x[10] - .07*x[11] +
    .25*x[13]^2 - .04*x[14] + .06*x[15] +
    -.01*x[17] - .03*x[18]
}






#' robotarm: Robot arm function.
#' 8 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' robotarm(runif(8))
#' robotarm(matrix(runif(8*20),ncol=8))
robotarm <- function(x, scale_it=T, scale_low = rep(0,8), scale_high = c(rep(2*pi,4),rep(1,4)), noise=0) {
  test_func_apply(func=TF_robotarm, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}

#' TF_robotarm: Robot arm function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_robotarm(rep(0,8))
#' TF_robotarm(rep(1,8))
TF_robotarm <- function(x) {
  theta <- x[1:4]
  L <- x[5:8]
  u <- sum(L * cos(cumsum(theta)))
  v <- sum(L * sin(cumsum(theta)))
  sqrt(u^2 + v^2)
}





#' RoosArnold: Roos & Arnold (1963) function.
#' d dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' RoosArnold(runif(8))
#' RoosArnold(matrix(runif(8*20),ncol=8))
RoosArnold <- function(x, scale_it=F, scale_low = 0, scale_high = 1, noise=0) {
  test_func_apply(func=TF_RoosArnold, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise)
}

#' TF_RoosArnold: Roos & Arnold (1963) function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_RoosArnold(rep(0,8))
#' TF_RoosArnold(rep(1,8))
TF_RoosArnold <- function(x) {
  prod(abs(4*x-2))
}





#' Gfunction: G-function
#' d dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' Gfunction(runif(8))
#' Gfunction(matrix(runif(8*20),ncol=8))
Gfunction <- function(x, scale_it=F, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_Gfunction, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_Gfunction: G-function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#' @param a Parameter for Gfunction
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_Gfunction(rep(0,8))
#' TF_Gfunction(rep(1,8))
TF_Gfunction <- function(x, a=(1:length(x)-1)/2) {
  prod((abs(4*x-2) + a) / (1 + a))
}






#' beale: Beale function
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' beale(runif(2))
#' beale(matrix(runif(2*20),ncol=2))
beale <- function(x, scale_it=T, scale_low = -4.5, scale_high = 4.5, noise=0, ...) {
  test_func_apply(func=TF_beale, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_beale: Beale function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_beale(rep(0,2))
#' TF_beale(rep(1,2))
TF_beale <- function(x) {
  (1.5-x[1]+x[1]*x[2])^2 +
    (2.25-x[1]+x[1]*x[2]^2)^2 +
    (2.625 - x[1] + x[1]*x[2]^3)^2
}







#' easom: Easom function
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' easom(runif(2))
#' easom(matrix(runif(2*20),ncol=2))
easom <- function(x, scale_it=T, scale_low = -4.5, scale_high = 4.5, noise=0, ...) {
  test_func_apply(func=TF_easom, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_easom: Easom function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_easom(rep(0,2))
#' TF_easom(rep(1,2))
TF_easom <- function(x) {
  -cos(x[1]) * cos(x[2]) * exp(-(x[1]-pi)^2 - (x[2]-pi)^2)
}








#' griewank: Griewank function
#' n dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' griewank(runif(2))
#' griewank(matrix(runif(2*20),ncol=2))
griewank <- function(x, scale_it=T, scale_low = -600, scale_high = 600, noise=0, ...) {
  test_func_apply(func=TF_griewank, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_griewank: Griewank function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_griewank(rep(0,2))
#' TF_griewank(rep(1,2))
TF_griewank <- function(x) {
  sum(x^2) / 400 - prod(cos(x/sqrt(1:length(x)))) + 1
}









#' hump: Hump function
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' hump(runif(2))
#' hump(matrix(runif(2*20),ncol=2))
hump <- function(x, scale_it=T, scale_low = -5, scale_high = 5, noise=0, ...) {
  test_func_apply(func=TF_hump, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_hump: Hump function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_hump(rep(0,2))
#' TF_hump(rep(1,2))
TF_hump <- function(x) {
  1.0316285 + 4*x[1]^2 -2.1*x[1]^4 + x[1]^6/3 + x[1]*x[2] - 4*x[2]^2 + 4*x[2]^4
}










#' levy: Levy function
#' n dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' levy(runif(2))
#' levy(matrix(runif(2*20),ncol=2))
levy <- function(x, scale_it=T, scale_low = -10, scale_high = 10, noise=0, ...) {
  test_func_apply(func=TF_levy, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_levy: Levy function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_levy(rep(0,2))
#' TF_levy(rep(1,2))
TF_levy <- function(x) {
  w <- 1 + (x-1) / 4
  d <- length(w)
  sin(pi*w[1])^2 +
    sum((w[-d] - 1)^2 * (1 + 10*sin(pi*w[-d]+1)^2)) +
    (w[d]-1)^2 * (1 + sin(2*pi*w[d])^2)
}











#' michalewicz: Michalewicz function
#' n dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' michalewicz(runif(2))
#' michalewicz(matrix(runif(2*20),ncol=2))
michalewicz <- function(x, scale_it=T, scale_low = 0, scale_high = pi, noise=0, ...) {
  test_func_apply(func=TF_michalewicz, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_michalewicz: Michalewicz function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#' @param m Parameter for the michalewicz function
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_michalewicz(rep(0,2))
#' TF_michalewicz(rep(1,2))
TF_michalewicz <- function(x, m=10) {
  -sum(sin(x) * sin(1:length(x) * x^2 / pi)^(2*m))
}












#' rastrigin: Rastrigin function
#' n dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' rastrigin(runif(2))
#' rastrigin(matrix(runif(2*20),ncol=2))
rastrigin <- function(x, scale_it=T, scale_low = -5.12, scale_high = 5.12, noise=0, ...) {
  test_func_apply(func=TF_rastrigin, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_rastrigin: Rastrigin function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_rastrigin(rep(0,2))
#' TF_rastrigin(rep(1,2))
TF_rastrigin <- function(x) {
  10*length(x) + sum(x^2 - 10*cos(2*pi*x))
}





#' moon_high: Moon (2010) high-dimensional function for screening
#' 20 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' moon_high(runif(20))
#' moon_high(matrix(runif(20*20),ncol=20))
moon_high <- function(x, scale_it=F, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_moon_high, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_moon_high: Moon (2010) high-dimensional function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_moon_high(rep(0,20))
#' TF_moon_high(rep(1,20))
TF_moon_high <- function(x) {

  coeff_linear <- c(-2.08, 2.11, 0.76, -0.57, -0.72, -0.47, 0.39, 1.40, -0.09, -0.70, -1.27, -1.03, 1.07, 2.23, 2.46, -1.31, -2.94, 2.63, 0.07, 2.44)

  coeff_quadratic <- matrix(0, 20, 20)
  coeff_quadratic[,1]  <- c(1.42,  2.18, 0.58, -1.21, -7.15, -1.29, -0.19, -2.75, -1.16, -1.09,  0.89, -0.16,  4.43,  1.65, -1.25, -1.35,  1.15, -19.71,  23.72,  1.42)
  coeff_quadratic[,2]  <- c(   0, -1.70, 0.84,  1.20, -2.35, -0.16, -0.19, -5.93, -1.15,  1.89, -3.47, -0.07, -0.60, -1.09, -3.23,  0.44,  1.24,   2.13,  -0.71,  1.64)
  coeff_quadratic[,3]  <- c(   0,     0, 1.00, -0.49,  1.74,  1.29, -0.35, -4.73,  3.27,  1.87,  1.42, -0.96, -0.91,  2.06,  2.89,  0.25,  1.97,   3.04,   2.00,  1.64)
  coeff_quadratic[,4]  <- c(   0,     0,    0, -3.23,  2.75, -1.40,  0.24, -0.70, -0.17, -3.38, -1.87, -0.17,  1.56,  2.40, -1.70,  0.32,  2.11,  -0.20,   1.39, -2.01)
  coeff_quadratic[,5]  <- c(   0,     0,    0,     0, -1.10,  2.34, -3.90, -0.80,  0.13, -3.97,  1.99,  0.45,  1.77, -0.50,  1.86,  0.02, -2.08,  -1.78,   1.76,  1.30)
  coeff_quadratic[,6]  <- c(   0,     0,    0,     0,     0,  0.21, -0.03, -0.37, -1.27,  2.78,  1.37, -2.75, -3.15,  1.86,  0.12, -0.74,  1.06,  -3.76,  -0.43,  1.25)
  coeff_quadratic[,7]  <- c(   0,     0,    0,     0,     0,     0, -4.16,  0.26, -0.30, -2.69, -2.56, 28.99, -2.13,  1.36,  1.45,  3.09, -1.73,  -1.66,  -3.94, -2.56)
  coeff_quadratic[,8]  <- c(   0,     0,    0,     0,     0,     0,     0, -1.00,  0.77,  1.09, -1.15, -1.09, -2.74,  1.59,  1.41,  0.48,  2.16,   0.34,   4.17,  0.73)
  coeff_quadratic[,9]  <- c(   0,     0,    0,     0,     0,     0,     0,     0,  3.06,  2.46,  5.80, -5.15, -2.05,  3.17,  3.40, -0.49, -6.71,  -0.74,   2.78, -0.41)
  coeff_quadratic[,10] <- c(   0,     0,    0,     0,     0,     0,     0,     0,     0,  3.34,  2.36, -1.77, -3.16,  1.89,  2.20, -0.71, -3.78,   0.98,   1.40, -0.59)
  coeff_quadratic[,11] <- c(   0,     0,    0,     0,     0,     0,     0,     0,     0,     0, -1.17, -2.45,  6.04,  3.22,  0.19, -0.03, -2.65,  -1.02,  -1.96, -2.66)
  coeff_quadratic[,12] <- c(   0,     0,    0,     0,     0,     0,     0,     0,     0,     0,     0,  1.52,  1.36, -0.59, -1.05, -0.84, -1.30,   0.42,   1.86, -0.32)
  coeff_quadratic[,13] <- c(   0,     0,    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,  0.42, -0.50,  0.21, -0.18,  3.04,  -0.53,  -0.12,  0.09)
  coeff_quadratic[,14] <- c(   0,     0,    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0, -1.13, -2.42, -3.93, -2.30,   0.40,   0.81, -1.10)
  coeff_quadratic[,15] <- c(   0,     0,    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0, -0.26,  5.31,  1.66,  -3.10,   3.37,  4.32)
  coeff_quadratic[,16] <- c(   0,     0,    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0, -2.26,  0.00,  -0.77,  -3.90, -1.08)
  coeff_quadratic[,17] <- c(   0,     0,    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,  0.62,  -1.06,  -0.86,  0.44)
  coeff_quadratic[,18] <- c(   0,     0,    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,   0.35,  -1.99,  1.50)
  coeff_quadratic[,19] <- c(   0,     0,    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,      0, -13.34,  1.34)
  coeff_quadratic[,20] <- c(   0,     0,    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,      0,      0, -0.38)

  sum(coeff_linear * x) + sum(coeff_quadratic * x%*%t(x))
}






#' linkletter_nosignal: Linkletter (2006) no signal function, just returns zero
#' d dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' linkletter_nosignal(runif(2))
#' linkletter_nosignal(matrix(runif(2*20),ncol=2))
linkletter_nosignal <- function(x, scale_it=F, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_linkletter_nosignal, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_linkletter_nosignal: Linkletter (2006) no signal function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_linkletter_nosignal(rep(0,2))
#' TF_linkletter_nosignal(rep(1,2))
TF_linkletter_nosignal <- function(x) {
  0
}








#' Morris: Morris function
#' 20 dimensional function.
#' @export
#' @rdname test_func_apply
#' @references http://www.abe.ufl.edu/jjones/ABE_5646/2010/Morris.1991%20SA%20paper.pdf
#' @examples
#' morris(runif(20))
#' morris(matrix(runif(20*20),ncol=20))
morris <- function(x, scale_it=T, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_morris, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_morris: morris function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @references http://www.abe.ufl.edu/jjones/ABE_5646/2010/Morris.1991%20SA%20paper.pdf
#' @examples
#' TF_morris(rep(0,20))
#' TF_morris(rep(1,20))
TF_morris <- function(x) {
  beta1 <- (-1)^(1:20)
  beta1[1:10] <- 20
  beta2 <- outer(1:20,1:20,Vectorize(function(i,j) {if (i<j) (-1)^(i+j) else 0}))
  beta2[1:6, 1:6] <- -15
  #beta3 <- array(0, c(20,20,20)) # Changed it to be 10x faster below since most terms are zero and outer is slow
  #for (i in 1:3) {
  #  for (j in (i+1):4) {
  #    for (k in (j+1):5) {
  #      beta3[i, j, k] <- -10
  #    }
  #  }
  #}
  w <- 2*(x-.5)
  w[c(3,5,7)] <- 2*(1.1*x[c(3,5,7)]/(x[c(3,5,7)]+.1) - .5)
  t3 <- -10 * sum(w[1]*(w[2]*w[3] + w[2]*w[4] + w[2]*w[5] + w[3]*w[4] + w[3]*w[5] + w[4]*w[5]) +
                    w[2]*(w[3]*w[4] + w[3]*w[5] + w[4]*w[5]) +
                    w[3]*w[4]*w[5])
  #sum(beta1 * w) + sum(beta2 * outer(w, w)) + sum(beta3 * outer(w, outer(w, w))) + 5*prod(w[1:4])
  sum(beta1 * w) + sum(beta2 * outer(w, w)) + t3 + 5*prod(w[1:4])
}



#' detpep8d: detpep8d function
#' 8 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' detpep8d(runif(2))
#' detpep8d(matrix(runif(2*20),ncol=2))
#' @references
#' http://www.tandfonline.com/doi/pdf/10.1198/TECH.2010.09157?needAccess=true
detpep8d <- function(x, scale_it=T, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_detpep8d, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_detpep8d: detpep8d function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_detpep8d(rep(0,2))
#' TF_detpep8d(rep(1,2))
TF_detpep8d <- function(x) {
  4*(x[1]-2+8*x[2]-8*x[2]^2)^2 + (3-4*x[2])^2 + 16*sqrt(x[3]+1)*(2*x[3]-1)^2 + sum((4:8) * log(1 + x[3] + cumsum(x[4:8])))
}





#' hartmann: hartmann function
#' 6 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' hartmann(runif(2))
#' hartmann(matrix(runif(6*20),ncol=6))
hartmann <- function(x, scale_it=F, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_hartmann, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_hartmann: hartmann function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_hartmann(rep(0,6))
#' TF_hartmann(rep(1,6))
#' TF_hartmann(c(.20169, .150011, .476874, .275332, .311652, .6573)) # Global minimum of -3.322368
TF_hartmann <- function(x) {#browser()
  alpha <- c(1, 1.2, 3, 3.2)
  A <- matrix(c(10,3,17,3.5,1.7,8,
                .05,10,17,.1,8,14,
                3,3.5,1.7,10,17,8,
                17,8,.05,10,.1,14),
              nrow=4, ncol=6, byrow=T)
  P <- 1e-4*matrix(c(1312,1696,5569, 124,8283,5886,
                     2329,4135,8307,3736,1004,9991,
                     2348,1451,3522,2883,3047,6650,
                     4047,8828,8732,5743,1091, 381),
                   nrow=4,ncol=6,byrow=T)
  -sum(alpha * exp(-rowSums(A * sweep(P, 2, x)^2)))
}


#' quad_peaks: quad_peaks function
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' quad_peaks(runif(2))
#' quad_peaks(matrix(runif(2*20),ncol=2))
quad_peaks <- function(x, scale_it=T, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_quad_peaks, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_quad_peaks: quad_peaks function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_quad_peaks(rep(0,2))
#' TF_quad_peaks(rep(1,2))
TF_quad_peaks <- function(x) {
  .2+.015*TestFunctions::add_zoom(
    TestFunctions::rastrigin, scale_low = c(.4,.4), scale_high = c(.6,.6)
  )(x)^.9
}


#' quad_peaks_slant: quad_peaks_slant function
#' 2 dimensional function.
#' @export
#' @rdname test_func_apply
#' @examples
#' quad_peaks_slant(runif(2))
#' quad_peaks_slant(matrix(runif(2*20),ncol=2))
quad_peaks_slant <- function(x, scale_it=T, scale_low = 0, scale_high = 1, noise=0, ...) {
  test_func_apply(func=TF_quad_peaks_slant, x=x, scale_it=scale_it, scale_low = scale_low, scale_high = scale_high, noise=noise, ...)
}

#' TF_quad_peaks_slant: quad_peaks_slant function for evaluating a single point.
#'
#' @param x Input vector at which to evaluate.
#'
#' @return Function output evaluated at x.
#' @export
#'
#' @examples
#' TF_quad_peaks_slant(rep(0,2))
#' TF_quad_peaks_slant(rep(1,2))
TF_quad_peaks_slant <- function(x) {
  .2+.015*TestFunctions::add_zoom(
    TestFunctions::rastrigin, scale_low = c(.4,.4), scale_high = c(.6,.6)
  )(x)^.9 +
    sum(c(.01,.01) * x)
}
