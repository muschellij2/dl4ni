context("subset_problem")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("subset_problem works as expected", {

  # We'll use a modified BET (non-convolutional) demo
  load_keras()
  
  # Get the dataset
  problem <- "segmentation"
  problem_path <- problem %>% get_dataset()
  info <- problem_path %>% get_problem_info(num_subjects = 5, interactive = FALSE)
  
  info %>% split_train_test_sets()
  
  expect_works(info %>% subset_problem(subset_classes = c(1, 2, 3), unify_classes = c(2, 3), use_all = TRUE))
  expect_works(info %>% subset_problem(subset_classes = c(1, 2, 3), unify_classes = c(2, 3), use_all = FALSE))
  expect_works(info %>% subset_problem(subset_classes = c(1, 2, 3), unify_classes = c(2, 3), use_all = TRUE))
  expect_works(info %>% subset_problem(subset_classes = c(2, 3), use_all = FALSE))
  
})
