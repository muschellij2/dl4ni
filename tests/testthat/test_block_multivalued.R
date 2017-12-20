context("block_multivalued")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("block_multivalued works as expected", {

  load_keras()
  
  input <- layer_input(shape = c(100)) 
  
  expect_works(output <- input %>% 
                 block_multivalued(hidden_layers = list(dense(10)), num_values = 5, units = 10))
  
  expect_works(model <- keras_model(input, output))
  
})
