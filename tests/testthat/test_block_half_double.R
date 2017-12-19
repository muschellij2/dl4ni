context("block_half_double")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("block_half_double works as expected", {
  
  load_keras()
  
  input <- layer_input(shape = c(16, 16, 16, 1))
  
  expect_works(output <- input %>% 
                 block_half(use_maxpooling = TRUE, batch_normalization = TRUE, dropout = 0.2) %>% 
                 block_half(use_maxpooling = FALSE, batch_normalization = TRUE, dropout = 0.2))
  expect_works(model <- keras_model(input, output))
  
  expect_works(output <- input %>% 
                 block_double(use_upsampling = TRUE, batch_normalization = TRUE, dropout = 0.2) %>% 
                 block_double(use_upsampling = FALSE, batch_normalization = TRUE, dropout = 0.2))
  expect_works(model <- keras_model(input, output))
  
  
})
