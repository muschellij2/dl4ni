reset_model <- function(.model) {
  
  stopifnot(inherits(.model, "DLmodel"))
  
  # The model to reset
  model <- .model$get_model()
  
  K <- keras::backend()
  
  # Get current backend session
  my_session <- K$get_session()
  
  # Reinitialize all weights in all layers
  for (layer in model$layers) {
    
    # The layer must have weights
    if (!is.null(layer$weights)) {
      
      for (w in layer$weights) {
        
        # And the weights must have a initilization method
        if (!is.null(w$initializer)) {
          
          w$initializer$run(session = my_session)
          
        }
        
      }
      
    }
    
  }
  
}
