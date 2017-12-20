context("scales")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("scales works as expected", {

  img <- array(runif(10 * 10 * 10), dim = c(10, 10, 10))
  
  expect_works(V <- scale_z(img))
  expect_equal(dim(V), c(10, 10, 10))
  
  expect_works(V <- scale_z_masked(img))
  expect_equal(dim(V), c(10, 10, 10))
  
  expect_works(V <- scale_max(img))
  expect_equal(dim(V), c(10, 10, 10))

  expect_works(V <- scale_max_masked(img))
  expect_equal(dim(V), c(10, 10, 10))
  
  expect_works(V <- scale_meanmax(img))
  expect_equal(dim(V), c(10, 10, 10))
  
  expect_works(V <- scale_meanmax_masked(img))
  expect_equal(dim(V), c(10, 10, 10))
  
})
