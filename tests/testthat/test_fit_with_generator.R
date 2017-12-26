context("fit_with_generator")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("fit_with_generator works as expected", {

  # We'll use a modified BET (non-convolutional) demo
  load_keras()
  
  # Get the dataset
  problem <- "brain_extraction"
  problem_path <- problem %>% get_dataset()
  info <- problem_path %>% get_problem_info(num_subjects = 5, interactive = FALSE)
  
  info %>% split_train_test_sets()
  
  # Model scheme
  scheme <- DLscheme$new()
  
  scheme$add(width = 7,
             only_convolutionals = FALSE,
             output_width = 3,
             num_features = 3,
             vol_layers_pattern = list(dense(25)),
             vol_dropout = 0.15,
             feature_layers = list(dense(10)),
             feature_dropout = 0.15,
             common_layers = list(dense(20)),
             common_dropout = 0.25,
             last_hidden_layers = list(dense(10)),
             optimizer = "adadelta",
             scale = "z",
             scale_y = "none")
  
  scheme$add(memory_limit = "1G")
  
  # Network instatiation
  expect_works(bet_model <- scheme$instantiate(problem_info = info))

  expect_error(bet_model %>% fit_with_generator())
  expect_warning(bet_model %>% fit_with_generator(train_config = bet_model$.__enclos_env__$private$train_config,
                                                  epochs = 2, verbose = FALSE,
                                                  reset_optimizer = TRUE,
                                                  metrics_viewer = TRUE))
  

})
