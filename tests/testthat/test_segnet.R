context("segnet")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("segnet works as expected", {

  expect_works(s1 <- segnet(mode = "sampling"))
  expect_works(s1 %>% normalize_layers())
  
  expect_works(s2 <- segnet(mode = "convolutional"))
  expect_works(s2 %>% normalize_layers())

})
