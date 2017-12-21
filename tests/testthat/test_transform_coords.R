context("transform_coords")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("transform_coords works as expected", {

  dims_x <- c(10, 10, 10)
  dims_y <- c(20, 20, 20)
  Vx <- array(runif(prod(dims_x)), dim = dims_x)
  Vy <- array(runif(prod(dims_y)), dim = dims_y)
  
  x <- seq(dims_x[1])
  y <- seq(dims_x[2])
  z <- seq(dims_x[3])
  
  expect_works(res <- transform_coords(x, y, z, Vx, Vy))
  
  expect_named(res, c("x", "y", "z"))

})
