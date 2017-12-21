context("get_problem_info")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("get_problem_info works as expected", {

  problem <- "brain_extraction"
  problem_path <- problem %>% get_dataset()
  
  expect_works(info <- problem_path %>% get_problem_info(num_subjects = 1, interactive = FALSE))
  expect_works(info <- problem_path %>% get_problem_info(num_subjects = 100, interactive = FALSE))
  
  expect_error(info2 <- get_problem_info(output_path = info$output_path))
  
  expect_works(info3 <- analyze_input(input = info$inputs)) %>% analyze_output(output = info$outputs)
  
})
