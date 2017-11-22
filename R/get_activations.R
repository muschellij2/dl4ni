#' @title Get Activations in Intermediate Layer
#'
#' @description This function computes the activation of an intermediate layer of a model.
#'
#' @param object     (\code{DLmodel} object) Base model.
#' @param layer      (integer or character) Name or index of the intermediate layer to compute its output.
#'
#' @return A function that computes the output of the layer given an input.
#' @export
#' @import keras
#' 
get_activations <- function(object, layer) {
  
  suppressPackageStartupMessages(require(keras))
  
  # Basic input checks
  stopifnot(inherits(object, "DLmodel"))
  
  model <- object$get_model()
  
  # Layer can be "layer_name" or "layer_index"
  # In both cases, we build a model with the same inputs as the original model
  # but the output is the one from the given layer 
  if (is.character(layer)) {
    
    m <- keras_model(inputs = model$input,
                     outputs = model$get_layer(name = layer)$output)
    
  } else {
    
    m <- keras_model(inputs = model$input,
                     outputs = model$get_layer(index = as.integer(layer))$output)
    
  }
  
  # This function is just the predictor
  f <- function(x) {
    
    m %>% set_trainability(trainability = FALSE)
    res <- m %>% predict_on_batch(x)
    m %>% set_trainability(trainability = TRUE)
    
    return(res)
    
  }
    
  return(f)
  
}
