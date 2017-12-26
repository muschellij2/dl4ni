context("dice_coef")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("dice_coef works as expected", {
  
  load_keras()
  
  input <- layer_input(shape = c(3))
  
  output <- input %>% 
    layer_dense(units = 100, activation = "relu") %>% 
    layer_dense(units = 50, activation = "sigmoid")
  
  m <- keras_model(input, output)
  
  expect_works(m %>% compile(optimizer = optimizer_nadam(), loss = bce_dice_loss))
  
  expect_works(m %>% fit(x = matrix(runif(100 * 3), ncol = 3), 
                         y = matrix(sample(c(0, 1), size = 100 * 50, replace = TRUE), ncol = 50), 
                         epochs = 1, verbose = 0))
  
  expect_works(res <- m %>% evaluate(x = matrix(runif(100 * 3), ncol = 3), 
                                     y = matrix(sample(c(0, 1), size = 100 * 50, replace = TRUE), ncol = 50),
                                     verbose = 0))
  
})
