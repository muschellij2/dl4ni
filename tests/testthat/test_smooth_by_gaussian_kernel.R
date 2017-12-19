context("smooth_by_gaussian_kernel")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("smooth_by_gaussian_kernel works as expected", {

  kernel_width <- 3
  
  I <- array(rnorm(10 * 10 * 10), dim = c(10, 10, 10))
  
  expect_works(img <- smooth_by_gaussian_kernel(image = I, kernel_sigma = 2, kernel_width = 3))
  
  expect_works(img <- smooth_by_gaussian_kernel(image = I, kernel_sigma = 2, kernel_width = 0))
  expect_identical(img, I)
  
  I <- array(rnorm(10 * 10 * 10 * 10), dim = c(10, 10, 10, 10))
  
  expect_works(img <- smooth_by_gaussian_kernel(image = I, kernel_sigma = 2, kernel_width = 3))
  

})
