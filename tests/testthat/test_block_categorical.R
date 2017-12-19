context("block_categorical")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("block_categorical works as expected", {

  load_keras()
  
  input <- layer_input(shape = c(100)) 
  
  expect_works(output <- input %>% 
                 block_categorical(hidden_layers = list(dense(10)), num_classes = 5, units = 10, concatenate = TRUE) %>% 
                 block_categorical(hidden_layers = list(dense(10)), num_classes = 5, units = 10, concatenate = FALSE))
  
  expect_works(model <- keras_model(input, output))

})
