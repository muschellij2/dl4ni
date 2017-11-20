#' Compute Model Total Size
#' @description This function computes the total size of a model, in bytes, for trainable parameters
#' and the input data.
#'
#' @param model   (\code{DLmodel} object) Model for whcih to compute the optimal batch_size
#'
#' @return Total model size in bytes
#' @export
#' 
model_size <- function(model) {
  
  # Number of parameters to train and size of input data
  data_size <- sum(model$get_model() %>% model_units())
  params_size <- model$get_model()$count_params()
  
  # Total size
  total_size <- object.size(vector(mode = "double", length = data_size + params_size))
  
  return(total_size)
  
}

#' Compute Optimal Batch Size
#' @description This function computes an estimate of the optimal batch size to train a model.
#'
#' @param model   (\code{DLmodel} object) Model for whcih to compute the optimal batch_size
#'
#' @return The optimal batch_size
#' @export
#'
compute_batch_size <- function(model) {
  
  # Max batch_size (https://stackoverflow.com/questions/46654424/how-to-calculate-optimal-batch-size)
  config <- model$get_config()
  batch_size <- config$memory_limit / (4 * model_size(model))
  
  # Better if it is a power of two
  optimal_batch_size <- as.integer(2 ^ floor(log2(batch_size)))
  
  return(optimal_batch_size)
  
}
