test_that("Test functions", {
  expect_error(tmp <- borehole(runif(8)), NA)
  expect_is(tmp, "numeric")
  expect_length(tmp, 1)
  rm(tmp)
})
