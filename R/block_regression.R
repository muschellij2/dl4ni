#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param object               (name) PARAM_DESCRIPTION
#' @param hidden_layers        (NULL) PARAM_DESCRIPTION, Default: list()
#' @param units                (numeric) PARAM_DESCRIPTION, Default: 1
#' @param output_activation    (character) PARAM_DESCRIPTION, Default: 'linear'
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso 
#'  
#' @export 
#' @import keras
block_regression <- function(object, 
                             hidden_layers = list(),
                             units = 1,
                             output_activation = "linear") {
  
  
  require(keras)
  
  # Final layer in each path
  finalize_layers <- list(dense(units = 1, 
                                activation = output_activation,
                                batch_normalization = FALSE))
  
  # Build the independent paths.
  output <- object %>% block_paths(hidden_layers = hidden_layers, 
                                   num_paths = units,
                                   finalize_layers = finalize_layers,
                                   concatenate = TRUE)
  
  
  return(output)
  
}
