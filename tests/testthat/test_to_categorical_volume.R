context("to_categorical_volume")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("to_categorical_volume works as expected", {

  dims <- c(10, 10, 10)
  V <- array(sample(0:4, size = prod(dims), replace = TRUE), dim = dims)
  
  expect_works(V2 <- to_categorical_volume(V))
  expect_equal(dim(V2)[length(dim(V2))], 5)
  
  expect_works(V2 <- to_categorical_volume_cpp(V, 0:4))
  expect_equal(dim(V2)[length(dim(V2))], 5)
  
})
