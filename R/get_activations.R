get_activations <- function(object, layer) {
  
  suppressPackageStartupMessages(require(keras))
  
  # Basic input checks
  stopifnot(inherits(object, "DLmodel"))
  
  # Layer can be "layer_name" or "layer_index"
  # In both cases, we build a model with the same inputs as the original model
  # but the output is the one from the given layer 
  if (is.character(layer)) {
    
    m <- keras_model(inputs = object$model$input,
                     outputs = object$model$get_layer(name = layer)$output)
    
  } else {
    
    m <- keras_model(inputs = object$model$input,
                     outputs = object$model$get_layer(index = as.integer(layer))$output)
    
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
