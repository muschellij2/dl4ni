context("block_half_double")

test_that("block_half_double works as expected for one input", {
  
  load_keras()
  
  input <- layer_input(shape = c(16, 16, 16, 1))
  
  expect_works(output <- input %>% 
                 block_half(use_maxpooling = TRUE, batch_normalization = TRUE, dropout = 0.2) %>% 
                 block_half(use_maxpooling = FALSE, batch_normalization = TRUE, dropout = 0.2))
  expect_works(model <- keras_model(input, output))
  
  expect_works(output <- input %>% 
                 block_double(num_steps = 2, use_upsampling = TRUE, batch_normalization = TRUE, dropout = 0.2) %>% 
                 block_double(num_steps = 2, use_upsampling = FALSE, batch_normalization = TRUE, dropout = 0.2))
  expect_works(model <- keras_model(input, output))
  
  
})

test_that("block_half_double works as expected for shared layers", {
  
  load_keras()
  
  input <- list(layer_input(shape = c(16, 16, 16, 1)), layer_input(shape = c(16, 16, 16, 1)))
  
  # Must work :-)
  expect_works(output <- input %>% 
                 block_half(use_maxpooling = TRUE, batch_normalization = TRUE, dropout = 0.2) %>% 
                 block_half(use_maxpooling = FALSE, batch_normalization = TRUE, dropout = 0.2))
  
  # Check output shape is the same as input
  expect_equal(length(output), length(input))
  
  # Check that we can build models
  expect_works(model1 <- keras_model(input[[1]], output[[1]]))
  expect_works(model2 <- keras_model(input[[2]], output[[2]]))
  
  # Check that layers are shared (except input layers and other particular layer types).
  expect_shared_layers(model1, model2)
  
  
  # Must work :-)
  expect_works(output <- input %>% 
                 block_double(num_steps = 2, use_upsampling = TRUE, batch_normalization = TRUE, dropout = 0.2) %>% 
                 block_double(num_steps = 2, use_upsampling = FALSE, batch_normalization = TRUE, dropout = 0.2))
  
  # Check output shape is the same as input
  expect_equal(length(output), length(input))
  
  # Check that we can build models
  expect_works(model1 <- keras_model(input[[1]], output[[1]]))
  expect_works(model2 <- keras_model(input[[2]], output[[2]]))
  
  # Check that layers are shared (except input layers and other particular layer types).
  expect_shared_layers(model1, model2)
  
})
