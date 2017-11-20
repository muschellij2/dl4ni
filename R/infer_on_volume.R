infer_on_volume <- function(object, 
                            V = NULL, 
                            speed = c("faster", "medium", "slower"), 
                            ...) {
  
  # Basic input check
  stopifnot(inherits(object, "DLconfig") | inherits(object, "DLmodel"))
  
  if (inherits(object, "DLmodel")) {
    
    config <- object$get_config()
    model <- object
    
    infer <- config %>% create_inference_function_from_config()
    
    if (is.null(V)) {
      
      warning("The user didn't provide a volume. Returning a generic inference function.")
      return(infer)
      
    }

    res <- model %>% infer(V = V, speed = speed, ...)
    
    return(res)
    
  } else {
    
    config <- object
    
    infer <- config %>% create_inference_function_from_config()
    
    warning("Not a model. Returning a generic inference function.")
    return(infer)

  }
  
}
