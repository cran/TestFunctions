#' Wave functions
#'
#' nsin: Block wave
#'
#' @param xx Input values
#'
#' @return nsin evaluated at nsin
#' @export
#'
#' @examples
#' curve(nsin(2*pi*x), n = 1000)
#' curve(nsin(12*pi*x), n = 1000)
nsin <- function(xx) {sign(sin(xx))}#{-1+2*ceiling(sin(xx))} # block wave

#' vsin: v wave
#' @rdname nsin
#' @export
#' @examples
#' curve(vsin(2*pi*x), n = 1000)
#' curve(vsin(12*pi*x), n = 1000)
vsin <- function(xx) { # v/w wave
  yy <- xx%%(2*pi)
  ifelse(yy<pi/2,yy/(pi/2),0) +
    ifelse(yy>=pi/2 & yy<3*pi/2,2-yy/(pi/2),0) +
    ifelse(yy>=3*pi/2,-4+yy/(pi/2),0)
}


#' Evaluate an RFF (random wave function) at given input
#'
#' @param x Matrix whose rows are points to evaluate or a vector representing
#' a single point. In 1 dimension you must use a matrix for multiple points,
#' not a vector.
#' @param freq Vector of wave frequencies
#' @param mag Vector of wave magnitudes
#' @param dirr Matrix of wave directions
#' @param offset Vector of wave offsets
#' @param wave Type of wave
#' @param noise Standard deviation of random normal noise to add
#'
#' @return Output of RFF evaluated at x
#' @export
#'
#' @examples
#' curve(RFF(matrix(x,ncol=1),3,1,1,0))
#' curve(RFF(matrix(x,ncol=1),3,1,1,0, noise=.1), n=1e3, type='p', pch=19)
#'
#' curve(RFF(matrix(x,ncol=1),c(3,20),c(1,.1),c(1,1),c(0,0)), n=1e3)
RFF <- function(x,freq,mag,dirr,offset, wave=sin, noise=0) {
  #x <- matrix(x,ncol=2)
  (wave(2*pi* sweep(sweep(x %*% t(dirr),2,offset,'+'), 2,freq,'*')) %*% mag) +
    rnorm(if (is.matrix(x)) {nrow(x)} else {1},0,noise)
}

#' Create a new RFF function
#'
#' @param D Number of dimensions
#' @param M Number of random waves
#' @param wave Type of wave
#' @param noise Standard deviation of random normal noise to add
#'
#' @return A random wave function
#' @export
#'
#' @examples
#' func <- RFF_get(D=1)
#' curve(func)
#'
#' f <- RFF_get(D=1, noise=.1)
#' curve(f(matrix(x,ncol=1)))
#' for(i in 1:100) curve(f(matrix(x,ncol=1)), add=TRUE, col=sample(2:8,1))
RFF_get <- function(D=2, M=30, wave=sin, noise=0) {
  freq <- sort((rexp(M,1/7))) + 0.5 # can use ceiling to get ints, then don't add anything
  mag <- matrix(sapply(1:M,function(i){runif(1,-1/freq[i],1/freq[i])}), ncol=1)
  dirr <- matrix(runif(D*M,-1,1),ncol=D)
  dirrnorm <- apply(dirr,1,function(a)sqrt(sum(a^2)))
  dirr <- sweep(dirr,1,dirrnorm,'/')
  offset <- runif(M)
  if (is.character(wave)) {
    if (any(wave == c("sin"))) {wave <- sin}
    else if (any(wave == c("n","block","square","nsin"))) {wave <- nsin}
    else if (any(wave == c("v","vsin"))) {wave <- vsin}
    else {stop("wave not valid")}
  }
  function(x) {RFF(x, freq=freq, mag=mag, dirr=dirr, offset=offset, wave=wave, noise=noise)}
}
