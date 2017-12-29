context("unet")

test_that("conv_block works as expected for one input", {
  
  load_keras()
  
  input <- layer_input(shape = c(16, 16, 16, 1))
  
  expect_works(output <- input %>% 
                 conv_block(num_filters = 2, batch_normalization = TRUE, residual = TRUE, dropout = 0.2)
  )
  
  expect_works(model <- keras_model(input, output))
  
})

test_that("level_block works as expected for one input", {
  
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

test_that("block_unet works as expected for one input", {
  
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

test_that("conv_block works as expected for shared layers", {
  
  load_keras()
  
  input <- list(layer_input(shape = c(16, 16, 16, 1)), layer_input(shape = c(16, 16, 16, 1)))
  
  expect_works(output <- input %>% 
                 conv_block(num_filters = 2, batch_normalization = TRUE, residual = TRUE, dropout = 0.2)
  )
  
  expect_works(model1 <- keras_model(input[[1]], output[[1]]))
  expect_works(model2 <- keras_model(input[[2]], output[[2]]))
  
  expect_shared_layers(model1, model2)
  
})

test_that("level_block works as expected for shared layers", {
  
  load_keras()
  
  input <- list(layer_input(shape = c(16, 16, 16, 1)), layer_input(shape = c(16, 16, 16, 1)))
  
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
  
  expect_works(model1 <- keras_model(input[[1]], output[[1]]))
  expect_works(model2 <- keras_model(input[[2]], output[[2]]))
  
  expect_shared_layers(model1, model2)
  
})

test_that("block_unet works as expected for shared layers", {
  
  load_keras()
  
  input <- list(layer_input(shape = c(16, 16, 16, 1)), layer_input(shape = c(16, 16, 16, 1)))
  
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
  
  expect_works(model1 <- keras_model(input[[1]], output[[1]]))
  expect_works(model2 <- keras_model(input[[2]], output[[2]]))
  
  expect_shared_layers(model1, model2)
  
})
