context("block_clf")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("block_clf works as expected", {

  load_keras()
  
  input <- layer_input(shape = c(100)) 
  
  expect_works(output <- input %>% 
                 block_clf(hidden_layers = list(dense(10)), all = TRUE) %>% 
                 block_clf(hidden_layers = list(dense(10)), all = FALSE))
  
  expect_works(model <- keras_model(input, output))
  
})
