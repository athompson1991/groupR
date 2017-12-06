context("Utils")

test_that("get_combinations returns as expected", {
  x <- c("foo", "bar", "baz", "potato")
  y <- get_combinations(3, x)
  expect_equal(length(y), 3)
  expect_equal(dim(y[[1]]), c(1, 4))
  expect_equal(dim(y[[2]]), c(2, 6))
  expect_equal(dim(y[[3]]), c(3, 4))
})
