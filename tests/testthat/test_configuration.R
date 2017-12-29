context("configuration")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("configuration works as expected", {

  
  problem <- "brain_extraction"
  problem_path <- problem %>% get_dataset()
  info <- problem_path %>% get_problem_info(as_autoencoder = TRUE)
  
  info %>% split_train_test_sets()
  
  width <- 7
  scheme <- DLscheme$new()
  
  scheme$add(width = width,
             is_autoencoder = TRUE,
             only_convolutionals = FALSE,
             output_width = 3,
             num_features = 3,
             vol_layers_pattern = list(dense(25)),
             vol_dropout = 0.15,
             feature_layers = list(dense(5)),
             feature_dropout = 0.15,
             common_layers = list(dense(25)),
             common_dropout = 0.25,
             decoder_layers = list(dense(10)),
             last_hidden_layers = list(dense(20), dense(10)),
             optimizer = "adadelta",
             scale = "meanmax")
  
  scheme$add(memory_limit = "1G")
  
  expect_works(ae_model <- info %>% scheme$instantiate())
  
  
})
