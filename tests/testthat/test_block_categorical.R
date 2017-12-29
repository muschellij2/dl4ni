context("block_categorical")

test_that("block_categorical works as expected for one input", {

  load_keras()
  
  input <- layer_input(shape = c(100)) 
  
  expect_works(output <- input %>% 
                 block_categorical(hidden_layers = list(dense(10)), num_classes = 5, units = 10, concatenate = TRUE) %>% 
                 block_categorical(hidden_layers = list(dense(10)), num_classes = 5, units = 10, concatenate = FALSE))
  
  expect_works(model <- keras_model(input, output))

})

test_that("block_categorical works as expected for shared layers", {
  
  load_keras()
  
  inputs <- list(layer_input(shape = c(100)), layer_input(shape = c(100))) 
  
  expect_works(output <- inputs %>% 
                 block_categorical(hidden_layers = list(dense(10)), num_classes = 5, units = 10, concatenate = TRUE) %>% 
                 block_categorical(hidden_layers = list(dense(10)), num_classes = 5, units = 10, concatenate = FALSE))
  
  expect_equal(length(output), length(inputs))
  
  expect_works(model1 <- keras_model(inputs[[1]], output[[1]]))
  expect_works(model2 <- keras_model(inputs[[2]], output[[2]]))
  
  # Check that layers are shared by looking at their names
  expect_shared_layers(model1, model2)

})
