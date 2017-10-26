#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param info           (name) PARAM_DESCRIPTION
#' @param train_split    (numeric) PARAM_DESCRIPTION, Default: 0.75
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
split_train_test_sets <- function(info, train_split = 0.75) {
  
  subject_index <- seq_along(info$inputs[[1]])
  
  train_size <- round(train_split * length(subject_index))
  
  train_indices <- sample(subject_index, size = train_size)
  test_indices <- setdiff(subject_index, train_indices)
  
  
  train <- list()
  train$subject_indices <- train_indices
  train$y <- info$outputs[train_indices]
  train$x <- list()
  for (input in seq(info$num_inputs)) {
    
    train$x[[input]] <- info$inputs[[input]][train_indices]
    
  }
  
  info$train <- train

  test <- list()
  test$subject_indices <- test_indices
  test$y <- info$outputs[test_indices]
  test$x <- list()
  for (input in seq(info$num_inputs)) {
    
    test$x[[input]] <- info$inputs[[input]][test_indices]
    
  }
  
  info$test <- test
  
  invisible(NULL)
  
}
