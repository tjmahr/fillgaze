context("set_values_to_na()")

test_that("set_values_to_na() handles formulas and named functions", {
  df <- data.frame(
    x = -2:2,
    y = 1:5,
    z = c(Inf, Inf, 5:7),
    q = 11:15)

  f1 <- ~ .x < 0
  f2 <- function(x) x < 3
  f3 <- is.infinite

  result <- set_values_to_na(df, x = f1, y = f2, z = f3, q = f1)
  testthat::expect_equal(result$x, c(NA, NA, 0, 1, 2))
  testthat::expect_equal(result$y, c(NA, NA, 3, 4, 5))
  testthat::expect_equal(result$z, c(NA, NA, 5, 6, 7))

  # q tests that values that should not be converted are not converted
  testthat::expect_equal(result$q, 11:15)
})
