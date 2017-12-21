context("meshnet")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("meshnet works as expected", {

  expect_works(meshnet(num_filters = 1, num_blocks = 7, dropout = 0.1, out_filters = 2) %>% normalize_layers())
  
})
