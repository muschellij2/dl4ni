context("parse_optimizer")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("parse_optimizer works as expected", {


  expect_works(parse_optimizer("nadam"))
  expect_error(parse_optimizer("foo"))
  
  expect_works(parse_optimizer(NULL))
  expect_error(parse_optimizer(list(1)))
  expect_works(parse_optimizer(optimizer_nadam()))
  
})
