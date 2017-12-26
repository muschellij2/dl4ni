context("utils")


test_that("utils works as expected", {

  
  expect_equal(convert_to_bytes("1G"), 1024 * 1024 * 1024)
  
  expect_equal(round_to_multiple(100), 32 * 3)
  expect_equal(round_to_multiple(100, upper = TRUE), 32 * 4)
  
  expect_equal(round_to_power2(10), 8)
  expect_equal(round_to_power2(100), 64)
  
})
