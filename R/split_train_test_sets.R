#' @title Split Problem into Train and Test Sets
#'
#' @description This function is used to split inputs and outputs of a problem definition into train and test sets.
#'
#' @param info           (\code{DLproblem} object, as created by \code{\link{get_rpoblem_info}}) The information of the problem, with inputs and outputs.
#' @param train_split    (numeric) Fraction of the subjects to be used as training set, Default: 0.75
#'
#' @return The \code{DLproblem} completed with \code{train} and \code{test} sets.
#'
#' @export 
#' 
split_train_test_sets <- function(info, train_split = 0.75) {
  
  # Index of all subjects
  subject_index <- seq_along(info$inputs[[1]])
  
  # Number of subjects used for training
  train_size <- round(train_split * length(subject_index))
  
  # Randomly select subjects for train and test set
  train_indices <- sample(subject_index, size = train_size)
  test_indices <- setdiff(subject_index, train_indices)
  
  # Store training subjects in a list (both indices, inputs and outputs)
  train <- list()
  train$subject_indices <- train_indices
  train$y <- info$outputs[train_indices]
  train$x <- list()
  for (input in seq(info$num_inputs)) {
    
    train$x[[input]] <- info$inputs[[input]][train_indices]
    
  }
  
  info$train <- train

  # Store test subjects in a list (both indices, inputs and outputs)
  test <- list()
  test$subject_indices <- test_indices
  test$y <- info$outputs[test_indices]
  test$x <- list()
  for (input in seq(info$num_inputs)) {
    
    test$x[[input]] <- info$inputs[[input]][test_indices]
    
  }
  
  info$test <- test
  
  return(invisible(NULL))
  
}
