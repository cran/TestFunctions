random_LHS <- function(n, d) {
  lhsq <- (matrix(data=1:n, byrow=F, nrow=n, ncol=d) - 1 +
             matrix(data=runif(n*d), nrow=n, ncol=d)
  ) / n
  # Randomize each column
  for (i in 1:d) {
    lhsq[, i] <- lhsq[sample(1:n, n, replace=F), i]
  }
  lhsq
}
if (F) {
  random_LHS(10,3)
  stopifnot(sort(ceiling(random_LHS(10,3)*10)[,2]) == 1:10)
}
