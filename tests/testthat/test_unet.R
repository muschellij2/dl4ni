context("unet")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("conv_block works as expected", {
  
  load_keras()
  
  input <- layer_input(shape = c(16, 16, 16, 1))
  
  expect_works(output <- input %>% 
                 conv_block(num_filters = 2, batch_normalization = TRUE, residual = TRUE, dropout = 0.2)
  )
  
  expect_works(model <- keras_model(input, output))
  
})

test_that("level_block works as expected", {
  
  load_keras()
  
  input <- layer_input(shape = c(16, 16, 16, 1))
  
  expect_works(output <- input %>% 
                 level_block(depth = 2, 
                             num_filters = 2, 
                             batch_normalization = TRUE, 
                             residual = TRUE, 
                             dropout = 0.2, 
                             mode = "sampling") %>% 
                 level_block(depth = 2, 
                             num_filters = 2, 
                             batch_normalization = TRUE, 
                             residual = TRUE, 
                             dropout = 0.2, 
                             mode = "convolutional")
  )
  
  expect_works(model <- keras_model(input, output))
  
})

test_that("block_unet works as expected", {
  
  load_keras()
  
  input <- layer_input(shape = c(16, 16, 16, 1))
  
  expect_works(output <- input %>% 
                 block_unet(initial_filters = 2,
                            out_filters = 2,
                            depth = 2,
                            batch_normalization = TRUE, 
                            residual = TRUE, 
                            mode = "sampling") %>% 
                 block_unet(initial_filters = 2,
                            out_filters = 2,
                            depth = 2,
                            batch_normalization = TRUE, 
                            residual = TRUE,
                            mode = "convolutional")
  )
  
  expect_works(model <- keras_model(input, output))
  
})
