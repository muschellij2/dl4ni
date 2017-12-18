context("prefixed_schemes")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("prefixed_schemes works as expected", {

  width <- 32
  
  expect_works(SEGNET <- scheme_segnet(width = width, initial_filters = 1))
  SEGNET$add(memory_limit = "2G")
  expect_is(SEGNET, "DLscheme")
  
  expect_works(UNET <- scheme_unet(width = width, initial_filters = 1))
  UNET$add(memory_limit = "2G")
  expect_is(UNET, "DLscheme")
  
  expect_works(MESHNET <- scheme_meshnet(width = width, initial_filters = 1))
  MESHNET$add(memory_limit = "2G")
  expect_is(MESHNET, "DLscheme")
  
  # We'll use a modified BET (non-convolutional) demo
  load_keras()
  
  # Get the dataset
  problem <- "brain_extraction"
  problem_path <- problem %>% get_dataset()
  info <- problem_path %>% get_problem_info(num_subjects = 5, interactive = FALSE)
  
  info %>% split_train_test_sets()
  
  # Network instatiation
  expect_works(bet_model <- SEGNET$instantiate(problem_info = info))
  expect_is(bet_model, "DLmodel")
  
  expect_works(bet_model$use_data(use = "train", 
                                  x_files = info$train$x, 
                                  y_files = info$train$y, 
                                  target_windows_per_file = 1024))
  
  gen <- bet_model$.__enclos_env__$private$train_config$generator
  
  expect_works(data <- gen())
  
  # Network instatiation
  expect_works(bet_model <- UNET$instantiate(problem_info = info))
  expect_is(bet_model, "DLmodel")

  # Network instatiation
  expect_works(bet_model <- MESHNET$instantiate(problem_info = info))
  expect_is(bet_model, "DLmodel")
  
})
