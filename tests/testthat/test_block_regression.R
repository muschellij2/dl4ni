context("block_regression")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("block_regression works as expected", {

  load_keras()
  
  input <- layer_input(shape = c(100)) 
  
  expect_works(output <- input %>% 
                 block_regression(hidden_layers = list(dense(10)), output_activation = "relu"))
  
  expect_works(model <- keras_model(input, output))
})
