context("analyze_output")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("analyze_output works as expected", {

  # We'll use a modified BET (non-convolutional) demo
  load_keras()
  
  # Get the dataset
  problem <- "segmentation"
  problem_path <- problem %>% get_dataset()
  info <- problem_path %>% get_problem_info(num_subjects = 5, interactive = FALSE)
  
  expect_works(res <- analyze_output(output = info$outputs[1]))
  expect_is(res, "DLproblem")
  expect_named(res, c("type", "values", "remap_classes"), ignore.order = TRUE)
  expect_equal(res$type, "image_labelling")
  
  expect_works(res <- analyze_output(output = info$inputs$T1[1]))
  expect_is(res, "DLproblem")
  expect_named(res, c("type", "range"), ignore.order = TRUE)
  expect_equal(res$type, "image_regression")
  
  expect_works(res <- analyze_output(output = "foo.tmp"))
  expect_is(res, "DLproblem")
  expect_named(res, c("type"), ignore.order = TRUE)
  expect_equal(res$type, "subject_classification")
  
})
