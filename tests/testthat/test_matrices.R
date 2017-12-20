context("matrices")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("matrices works as expected", {

  # Identity matrix
  expect_works(id <- identity_matrix())
  
  expect_is(id, "matrix")
  expect_equal(nrow(id), 4)
  expect_equal(ncol(id), 4)

  # Translation matrix
  expect_works(m <- translation_matrix())
  
  expect_is(m, "matrix")
  expect_equal(nrow(m), 4)
  expect_equal(ncol(m), 4)
  
  # Rotation matrix
  expect_works(m <- rotation_matrix_along_axis())
  
  expect_is(m, "matrix")
  expect_equal(nrow(m), 4)
  expect_equal(ncol(m), 4)
  
  # Rotation matrix
  expect_works(m <- rotation_matrix())
  
  expect_is(m, "matrix")
  expect_equal(nrow(m), 4)
  expect_equal(ncol(m), 4)

  # Scale matrix
  expect_works(m <- scale_matrix())
  
  expect_is(m, "matrix")
  expect_equal(nrow(m), 4)
  expect_equal(ncol(m), 4)

  # Transformation matrix
  expect_works(m <- transformation_matrix())
  
  expect_is(m, "matrix")
  expect_equal(nrow(m), 4)
  expect_equal(ncol(m), 4)

  # Random transformation matrix
  expect_works(m <- random_transformation_matrix())
  
  expect_is(m, "matrix")
  expect_equal(nrow(m), 4)
  expect_equal(ncol(m), 4)
  
})
