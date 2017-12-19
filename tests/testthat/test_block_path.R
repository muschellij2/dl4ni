context("block_path")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("block_path works as expected", {
  
  load_keras()
  
  input <- layer_input(shape = c(100)) 
  
  expect_works(output <- input %>% 
                 block_paths(hidden_layers = list(dense(10)), num_paths = 5, concatenate = TRUE) %>% 
                 block_paths(hidden_layers = list(dense(10)), num_paths = 2, concatenate = FALSE))
  
  expect_works(model <- keras_model(input, output))
  
})
