#' Profile a function
#'
#' Gives details about how linear it is.
#'
#' @param func A function with a single output
#' @param d The number of input dimensions for the function
#' @param n The number of points to use for the linear model.
#' @param bins Number of bins in histogram.
#'
#' @importFrom stats sd predict lm
#' @importFrom graphics plot abline
#' @return Nothing, prints and plots
#' @export
#'
#' @examples
#' funcprofile(ackley, 2)
funcprofile <- function(func, d, n=1000*d, bins=30) {
  if (missing(d)) {stop("You must give in d, the number of input dimensions")}

  # func <- TestFunctions::borehole
  # d <- 8
  # n <- 1000*d
  x <- lhs::randomLHS(n, d)
  y <- apply(x, 1, func)
  df <- data.frame(x=x, y=y)
  n2 <- n #max(n, 1000*d)
  x2 <- lhs::randomLHS(n, d)
  y2 <- apply(x2, 1, func)
  df2 <- data.frame(x=x2, y=y2)


  # Calculate stats
  rng <- range(y)
  mn <- mean(y)
  std <- sd(y)
  cat("Summary of y:\n")
  print(c(summary(y), sd=std))
  cat("Showing histogram of y")
  print(ggplot2::ggplot(data.frame(y=y), ggplot2::aes_string(y)) +
          ggplot2::geom_histogram(bins=bins) +
          ggplot2::ggtitle("Histogram of y"))
  mod_lm <- lm(y ~ ., df)
  cat("Summary of linear model is:\n")
  print(summary(mod_lm))
  pred_lm_2 <- predict(mod_lm, df2)
  r2_2 <- 1 - sum((pred_lm_2 - y2)^2) / sum((mn - y2)^2)
  cat("R-squared on test data:", r2_2, "\n")

  plot(y,predict(mod_lm, data.frame(x=x)), main="LM predictions"); abline(a=0,b=1, col=2)

  if (FALSE) {
    # Quadratic model, looks awful
    mod_qm <- lm(y ~ poly(x, 2))
    print(summary(mod_qm)$r.squared)
    print(summary(mod_qm)$adj.r.squared)
    pred_qm_2 <- predict(mod_qm, data.frame(x=x2))
    r2_qm_2 <- 1 - sum((pred_qm_2 - y2)^2) / sum((mn - y2)^2)
    r2_qm_2
    # Quadratic model no interactions, still awful
    mod_qm <- lm(y ~ x + (x)^2)
    print(summary(mod_qm)$r.squared)
    print(summary(mod_qm)$adj.r.squared)
    pred_qm_2 <- predict(mod_qm, data.frame(x=x2))
    r2_qm_2 <- 1 - sum((pred_qm_2 - y2)^2) / sum((mn - y2)^2)
    r2_qm_2
  }

  # Grad norm 2
  if (TRUE) {
    g <- apply(x, 1, function(xi) {numDeriv::grad(func, xi)})
    g2 <- colMeans(g^2)
    print(ggplot2::ggplot(data.frame(g2=g2), ggplot2::aes_string(g2)) +
            ggplot2::geom_histogram(bins=bins) +
            ggplot2::ggtitle("Histogram of grad norm 2"))

  }
}
if (F) {
  funcprofile(TestFunctions::borehole, 8)

  rff1 <- TestFunctions::RFF_get(D=8)
  rff1 <- TestFunctions::borehole
  u <- lhs::maximinLHS(n=400, 8)
  v <- rff1(u)
  ug <- apply(u, 1, function(xi){numDeriv::grad(rff1, xi)})
  ug2 <- colSums(ug^2)
  hist(ug2)
}
