% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TF_bananagramacy2Dexp.R, R/TF_gramacy2Dexp.R,
%   R/TF_gramacy6D.R, R/functions1.R, R/functions2.R, R/functionsMultiOut1.R,
%   R/test_func_apply.R
\name{bananagramacy2Dexp}
\alias{bananagramacy2Dexp}
\alias{bananatimesgramacy2Dexp}
\alias{gramacy2Dexp}
\alias{gramacy2Dexp3hole}
\alias{gramacy6D}
\alias{branin}
\alias{borehole}
\alias{franke}
\alias{zhou1998}
\alias{currin1991}
\alias{currin1991b}
\alias{limpoly}
\alias{limnonpoly}
\alias{banana}
\alias{banana_grad}
\alias{gaussian1}
\alias{sinumoid}
\alias{waterfall}
\alias{sqrtsin}
\alias{powsin}
\alias{OTL_Circuit}
\alias{GoldsteinPrice}
\alias{GoldsteinPriceLog}
\alias{ackley}
\alias{piston}
\alias{wingweight}
\alias{welch}
\alias{robotarm}
\alias{RoosArnold}
\alias{Gfunction}
\alias{beale}
\alias{easom}
\alias{griewank}
\alias{hump}
\alias{levy}
\alias{levytilt}
\alias{michalewicz}
\alias{rastrigin}
\alias{moon_high}
\alias{linkletter_nosignal}
\alias{morris}
\alias{detpep8d}
\alias{hartmann}
\alias{quad_peaks}
\alias{quad_peaks_slant}
\alias{SWNExpCos}
\alias{logistic}
\alias{logistic15}
\alias{logistic_plateau}
\alias{vertigrad}
\alias{vertigrad_grad}
\alias{beambending}
\alias{chengsandu}
\alias{steelcolumnstress}
\alias{winkel}
\alias{boreholeMV}
\alias{test_func_apply}
\title{bananagramacy2Dexp: bananagramacy2Dexp function
6 dimensional function.
First two dimensions are banana function,
next two are the gramacy2Dexp function,
last two are null dimensions}
\usage{
bananagramacy2Dexp(
  x,
  scale_it = T,
  scale_low = 0,
  scale_high = 1,
  noise = 0,
  ...
)

bananatimesgramacy2Dexp(
  x,
  scale_it = T,
  scale_low = 0,
  scale_high = 1,
  noise = 0,
  ...
)

gramacy2Dexp(x, scale_it = T, scale_low = -2, scale_high = 6, noise = 0, ...)

gramacy2Dexp3hole(
  x,
  scale_it = T,
  scale_low = 0,
  scale_high = 1,
  noise = 0,
  ...
)

gramacy6D(x, scale_it = T, scale_low = 0, scale_high = 1, noise = 0, ...)

branin(
  x,
  scale_it = T,
  scale_low = c(-5, 0),
  scale_high = c(10, 15),
  noise = 0
)

borehole(
  x,
  scale_it = T,
  scale_low = c(0.05, 100, 63070, 990, 63.1, 700, 1120, 9855),
  scale_high = c(0.15, 50000, 115600, 1110, 116, 820, 1680, 12045),
  noise = 0
)

franke(x, scale_it = F, scale_low = c(0, 0), scale_high = c(1, 1), noise = 0)

zhou1998(x, scale_it = F, scale_low = c(0, 0), scale_high = c(1, 1), noise = 0)

currin1991(
  x,
  scale_it = F,
  scale_low = c(0, 0),
  scale_high = c(1, 1),
  noise = 0
)

currin1991b(
  x,
  scale_it = F,
  scale_low = c(0, 0),
  scale_high = c(1, 1),
  noise = 0
)

limpoly(x, scale_it = F, scale_low = c(0, 0), scale_high = c(1, 1), noise = 0)

limnonpoly(
  x,
  scale_it = F,
  scale_low = c(0, 0),
  scale_high = c(1, 1),
  noise = 0
)

banana(
  x,
  scale_it = T,
  scale_low = c(-20, -10),
  scale_high = c(20, 5),
  noise = 0
)

banana_grad(
  x,
  scale_it = T,
  scale_low = c(-20, -10),
  scale_high = c(20, 5),
  noise = 0
)

gaussian1(
  x,
  scale_it = F,
  scale_low = c(0, 0),
  scale_high = c(1, 1),
  noise = 0
)

sinumoid(x, scale_it = F, scale_low = c(0, 0), scale_high = c(1, 1), noise = 0)

waterfall(
  x,
  scale_it = F,
  scale_low = c(0, 0),
  scale_high = c(1, 1),
  noise = 0
)

sqrtsin(
  x,
  scale_it = F,
  scale_low = c(0, 0),
  scale_high = c(1, 1),
  noise = 0,
  freq = 2 * pi
)

powsin(
  x,
  scale_it = F,
  scale_low = c(0, 0),
  scale_high = c(1, 1),
  noise = 0,
  freq = 2 * pi,
  pow = 0.7
)

OTL_Circuit(
  x,
  scale_it = T,
  scale_low = c(50, 25, 0.5, 1.2, 0.25, 50),
  scale_high = c(150, 70, 3, 2.5, 1.2, 300),
  noise = 0
)

GoldsteinPrice(
  x,
  scale_it = T,
  scale_low = c(-2, -2),
  scale_high = c(2, 2),
  noise = 0
)

GoldsteinPriceLog(
  x,
  scale_it = T,
  scale_low = c(-2, -2),
  scale_high = c(2, 2),
  noise = 0
)

ackley(
  x,
  scale_it = T,
  scale_low = -32.768,
  scale_high = 32.768,
  noise = 0,
  a = 20,
  b = 0.2,
  c = 2 * pi
)

piston(
  x,
  scale_it = T,
  scale_low = c(30, 0.005, 0.002, 1000, 90000, 290, 340),
  scale_high = c(60, 0.02, 0.01, 5000, 110000, 296, 360),
  noise = 0
)

wingweight(
  x,
  scale_it = T,
  scale_low = c(150, 220, 6, -10, 16, 0.5, 0.08, 2.5, 1700, 0.025),
  scale_high = c(200, 300, 10, 10, 45, 1, 0.18, 6, 2500, 0.08),
  noise = 0
)

welch(x, scale_it = T, scale_low = c(-0.5), scale_high = c(0.5), noise = 0)

robotarm(
  x,
  scale_it = T,
  scale_low = rep(0, 8),
  scale_high = c(rep(2 * pi, 4), rep(1, 4)),
  noise = 0
)

RoosArnold(x, scale_it = F, scale_low = 0, scale_high = 1, noise = 0)

Gfunction(x, scale_it = F, scale_low = 0, scale_high = 1, noise = 0, ...)

beale(x, scale_it = T, scale_low = -4.5, scale_high = 4.5, noise = 0, ...)

easom(x, scale_it = T, scale_low = -4.5, scale_high = 4.5, noise = 0, ...)

griewank(x, scale_it = T, scale_low = -600, scale_high = 600, noise = 0, ...)

hump(x, scale_it = T, scale_low = -5, scale_high = 5, noise = 0, ...)

levy(x, scale_it = T, scale_low = -10, scale_high = 10, noise = 0, ...)

levytilt(x, scale_it = T, scale_low = 0, scale_high = 1, noise = 0, ...)

michalewicz(x, scale_it = T, scale_low = 0, scale_high = pi, noise = 0, ...)

rastrigin(
  x,
  scale_it = T,
  scale_low = -5.12,
  scale_high = 5.12,
  noise = 0,
  ...
)

moon_high(x, scale_it = F, scale_low = 0, scale_high = 1, noise = 0, ...)

linkletter_nosignal(
  x,
  scale_it = F,
  scale_low = 0,
  scale_high = 1,
  noise = 0,
  ...
)

morris(x, scale_it = T, scale_low = 0, scale_high = 1, noise = 0, ...)

detpep8d(x, scale_it = T, scale_low = 0, scale_high = 1, noise = 0, ...)

hartmann(x, scale_it = F, scale_low = 0, scale_high = 1, noise = 0, ...)

quad_peaks(x, scale_it = T, scale_low = 0, scale_high = 1, noise = 0, ...)

quad_peaks_slant(
  x,
  scale_it = T,
  scale_low = 0,
  scale_high = 1,
  noise = 0,
  ...
)

SWNExpCos(x, scale_it = T, scale_low = 0, scale_high = 1, noise = 0, ...)

logistic(x, scale_it = T, scale_low = 0, scale_high = 1, noise = 0, ...)

logistic15(x, scale_it = T, scale_low = 0, scale_high = 1, noise = 0, ...)

logistic_plateau(
  x,
  scale_it = T,
  scale_low = 0,
  scale_high = 1,
  noise = 0,
  ...
)

vertigrad(x, scale_it = T, scale_low = 0, scale_high = 1, noise = 0, ...)

vertigrad_grad(x, scale_it = T, scale_low = 0, scale_high = 1, noise = 0, ...)

beambending(
  x,
  scale_it = T,
  scale_low = c(10, 1, 0.1),
  scale_high = c(20, 2, 0.2),
  noise = 0,
  ...
)

chengsandu(x, scale_it = T, scale_low = 0, scale_high = 1, noise = 0, ...)

steelcolumnstress(
  x,
  scale_it = T,
  scale_low = c(330, 4e+05, 420000, 420000, 200, 10, 100, 10, 12600),
  scale_high = c(470, 6e+05, 780000, 780000, 400, 30, 500, 50, 29400),
  noise = 0,
  ...
)

winkel(x, scale_it = T, scale_low = 0, scale_high = 1, noise = 0, ...)

boreholeMV(
  x,
  NOD = 51,
  scale_it = T,
  scale_low = c(0.05, 100, 63070, 990, 63.1, 700, 1120, 9855),
  scale_high = c(0.15, 50000, 115600, 1110, 116, 820, 1680, 12045),
  noise = 0
)

test_func_apply(func, x, scale_it, scale_low, scale_high, noise = 0, ...)
}
\arguments{
\item{x}{Input value, either a matrix whose rows are points or
a vector for a single point. Be careful with 1-D functions.}

\item{scale_it}{Should the data be scaled from [0, 1]^D to
[scale_low, scale_high]? This means the input data is confined
to be in [0, 1]^D, but the function isn't.}

\item{scale_low}{Lower bound for each variable}

\item{scale_high}{Upper bound for each variable}

\item{noise}{If white noise should be added, specify the
standard deviation for normal noise}

\item{...}{Additional parameters for func}

\item{freq}{Wave frequency for sqrtsin and powsin}

\item{pow}{Power for powsin}

\item{a}{A constant for ackley()}

\item{b}{A constant for ackley()}

\item{c}{A constant for ackley()}

\item{NOD}{number of output dimensions}

\item{func}{A function to evaluate}
}
\value{
Function values at x
}
\description{
branin: A function.
2 dimensional function.
}
\examples{
bananagramacy2Dexp(runif(6))
bananagramacy2Dexp(matrix(runif(6*20),ncol=6))
bananatimesgramacy2Dexp(runif(6))
bananatimesgramacy2Dexp(matrix(runif(6*20),ncol=6))
gramacy2Dexp(runif(2))
gramacy2Dexp(matrix(runif(2*20),ncol=2))
gramacy2Dexp3hole(runif(2))
gramacy2Dexp3hole(matrix(runif(2*20),ncol=2))
gramacy6D(runif(6))
gramacy6D(matrix(runif(6*20),ncol=6))
branin(runif(2))
branin(matrix(runif(20), ncol=2))
borehole(runif(8))
borehole(matrix(runif(80), ncol=8))
franke(runif(2))
zhou1998(runif(2))
currin1991(runif(2))
currin1991b(runif(2))
limpoly(runif(2))
limnonpoly(runif(2))
banana(runif(2))
x <- y <- seq(0, 1, len=100)
z <- outer(x, y, Vectorize(function(a, b){banana(c(a, b))}))
contour(x, y, z)
banana_grad(runif(2))
x <- y <- seq(0, 1, len=100)
z <- outer(x, y, Vectorize(function(a, b){sum(banana_grad(c(a, b))^2)}))
contour(x, y, z)
gaussian1(runif(2))
sinumoid(runif(2))
x <- y <- seq(0, 1, len=100)
z <- outer(x, y, Vectorize(function(a, b){sinumoid(c(a, b))}))
contour(x, y, z)
waterfall(runif(2))
sqrtsin(runif(1))
curve(sqrtsin(matrix(x,ncol=1)))
powsin(runif(1))#,pow=2)
OTL_Circuit(runif(6))
OTL_Circuit(matrix(runif(60),ncol=6))
GoldsteinPrice(runif(2))
GoldsteinPrice(matrix(runif(60),ncol=2))
GoldsteinPriceLog(runif(2))
GoldsteinPriceLog(matrix(runif(60),ncol=2))
ackley(runif(2))
ackley(matrix(runif(60),ncol=2))
piston(runif(7))
piston(matrix(runif(7*20),ncol=7))
wingweight(runif(10))
wingweight(matrix(runif(10*20),ncol=10))
welch(runif(20))
welch(matrix(runif(20*20),ncol=20))
robotarm(runif(8))
robotarm(matrix(runif(8*20),ncol=8))
RoosArnold(runif(8))
RoosArnold(matrix(runif(8*20),ncol=8))
Gfunction(runif(8))
Gfunction(matrix(runif(8*20),ncol=8))
beale(runif(2))
beale(matrix(runif(2*20),ncol=2))
easom(runif(2))
easom(matrix(runif(2*20),ncol=2))
griewank(runif(2))
griewank(matrix(runif(2*20),ncol=2))
hump(runif(2))
hump(matrix(runif(2*20),ncol=2))
levy(runif(2))
levy(matrix(runif(2*20),ncol=2))
levytilt(runif(2))
levytilt(matrix(runif(2*20),ncol=2))
michalewicz(runif(2))
michalewicz(matrix(runif(2*20),ncol=2))
rastrigin(runif(2))
rastrigin(matrix(runif(2*20),ncol=2))
moon_high(runif(20))
moon_high(matrix(runif(20*20),ncol=20))
linkletter_nosignal(runif(2))
linkletter_nosignal(matrix(runif(2*20),ncol=2))
morris(runif(20))
morris(matrix(runif(20*20),ncol=20))
detpep8d(runif(2))
detpep8d(matrix(runif(2*20),ncol=2))
hartmann(runif(2))
hartmann(matrix(runif(6*20),ncol=6))
quad_peaks(runif(2))
quad_peaks(matrix(runif(2*20),ncol=2))
quad_peaks_slant(runif(2))
quad_peaks_slant(matrix(runif(2*20),ncol=2))
SWNExpCos(runif(2))
SWNExpCos(matrix(runif(2*20),ncol=2))
curve(logistic, from=-5,to=5)
curve(logistic(x,offset=.5, scl=15))
logistic(matrix(runif(20),ncol=1))
curve(logistic15)
curve(logistic15(x,offset=.25))
logistic15(matrix(runif(20),ncol=1))
curve(logistic_plateau(matrix(x,ncol=1)))
logistic_plateau(matrix(runif(20),ncol=1))
vertigrad(runif(2))
vertigrad(matrix(runif(2*20),ncol=2))
vertigrad_grad(runif(2))
vertigrad_grad(matrix(runif(2*20),ncol=2))
beambending(runif(3))
beambending(matrix(runif(3*20),ncol=3))
chengsandu(runif(2))
chengsandu(matrix(runif(2*20),ncol=2))
steelcolumnstress(runif(8))
steelcolumnstress(matrix(runif(8*20),ncol=8))
winkel(runif(2))
winkel(matrix(runif(2*20),ncol=2))
boreholeMV(runif(8))
boreholeMV(matrix(runif(80), ncol=8))
x <- matrix(seq(0,1,length.out=10), ncol=1)
y <- test_func_apply(sin, x, TRUE, 0, 2*pi, .05)
plot(x,y)
curve(sin(2*pi*x), col=2, add=TRUE)
}
\references{
Gramacy, Robert B., and Herbert KH Lee.
"Adaptive design and analysis of supercomputer experiments."
Technometrics 51.2 (2009): 130-145.

Gramacy, Robert B., and Herbert KH Lee.
"Adaptive design and analysis of supercomputer experiments."
Technometrics 51.2 (2009): 130-145.

Gramacy, Robert B., and Herbert KH Lee.
"Adaptive design and analysis of supercomputer experiments."
Technometrics 51.2 (2009): 130-145.

Dixon, L. C. W. (1978). The global optimization problem: an introduction. Towards Global Optimiation 2, 1-15.

Morris, M. D., Mitchell, T. J., & Ylvisaker, D. (1993). Bayesian design and analysis of computer experiments: use of derivatives in surface prediction. Technometrics, 35(3), 243-255.

Worley, Brian A. Deterministic uncertainty analysis. No. ORNL-6428. Oak Ridge National Lab., TN (USA), 1987.

Franke, R. (1979). A critical comparison of some methods for interpolation of scattered data. Monterey, California: Naval Postgraduate School. Page 13.

An, J., & Owen, A. (2001). Quasi-regression. Journal of complexity, 17(4), 588-607.

Currin, C., Mitchell, T., Morris, M., & Ylvisaker, D. (1991). Bayesian prediction of deterministic functions, with applications to the design and analysis of computer experiments. Journal of the American Statistical Association, 86(416), 953-963.

Currin, C., Mitchell, T., Morris, M., & Ylvisaker, D. (1991). Bayesian prediction of deterministic functions, with applications to the design and analysis of computer experiments. Journal of the American Statistical Association, 86(416), 953-963.

Lim, Yong B., Jerome Sacks, W. J. Studden, and William J. Welch.
"Design and analysis of computer experiments when the output is highly
correlated over the input space."
Canadian Journal of Statistics 30, no. 1 (2002): 109-126.

Lim, Yong B., Jerome Sacks, W. J. Studden, and William J. Welch.
"Design and analysis of computer experiments when the output is highly
correlated over the input space."
Canadian Journal of Statistics 30, no. 1 (2002): 109-126.

Haario, H., Saksman, E., & Tamminen, J. (1999). Adaptive proposal distribution for random walk Metropolis algorithm. Computational Statistics, 14(3), 375-396.

Joseph, V. R., Dasgupta, T., Tuo, R., & Wu, C. J. (2015). Sequential exploration of complex surfaces using minimum energy designs. Technometrics, 57(1), 64-74.

Ben-Ari, Einat Neumann, and David M. Steinberg. "Modeling data
from computer experiments: an empirical comparison of kriging with MARS and
projection pursuit regression." Quality Engineering 19.4 (2007): 327-338.

Kenett, Ron S., Shelemyahu Zacks, and Daniele Amberti.
Modern Industrial Statistics: with applications in R,
MINITAB and JMP. John Wiley & Sons, 2013.

Forrester, A., & Keane, A. (2008). Engineering design via surrogate modelling: a practical guide. John Wiley & Sons.

http://www.abe.ufl.edu/jjones/ABE_5646/2010/Morris.1991%20SA%20paper.pdf

http://www.tandfonline.com/doi/pdf/10.1198/TECH.2010.09157?needAccess=true

Santner, T. J., Williams, B. J., & Notz, W. (2003). The Design and Analysis of Computer Experiments. Springer Science & Business Media.

Cheng, Haiyan, and Adrian Sandu. "Collocation least-squares polynomial chaos method." In Proceedings of the 2010 Spring Simulation Multiconference, p. 80. Society for Computer Simulation International, 2010.

Kuschel, Norbert, and Rudiger Rackwitz. "Two basic problems in reliability-based structural optimization." Mathematical Methods of Operations Research 46, no. 3 (1997): 309-333.

Prikhodko, Pavel, and Nikita Kotlyarov. "Calibration of Sobol indices estimates in case of noisy output." arXiv preprint arXiv:1804.00766 (2018).

Winkel, Munir A., Jonathan W. Stallings, Curt B. Storlie, and
Brian J. Reich. "Sequential Optimization in Locally Important Dimensions."
arXiv preprint arXiv:1804.10671 (2018).

Morris, M. D., Mitchell, T. J., & Ylvisaker, D. (1993). Bayesian design and analysis of computer experiments: use of derivatives in surface prediction. Technometrics, 35(3), 243-255.

Worley, Brian A. Deterministic uncertainty analysis. No. ORNL-6428. Oak Ridge National Lab., TN (USA), 1987.
}
