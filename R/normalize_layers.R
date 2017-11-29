#' Normalize Layers 
#' 
#' @description This function takes as input a list of layers and checks that all are well-defined.
#'
#' @param layers_definition    (list) List of layers.
#'
#' @return The normalized list of layers.
#' @export
#' 
normalize_layers <- function(layers_definition) {
  
  # If layers_definition consists only in numbers, take them as dense layers with as
  # many units as each number indicates
  if (!is.list(layers_definition) && is.vector(layers_definition) && all(is.numeric(layers_definition))) {
    
    tmp <- layers_definition
    
    layers_definition <- list()
    if (length(tmp) > 0) {
      
      for (i in seq_along(tmp)) {
        
        layers_definition[[i]] <- dense(units = tmp[i])
        
      }
      
    }
    
  }
  
  # Now, check that the input is a list
  if (!is.list(layers_definition))
    layers_definition <- list(layers_definition)
  
  # Check that each item of the list is numeric or a list with at least "type" field.
  if (length(layers_definition) > 0) {
    
    for (i in seq_along(layers_definition)) {
      
      current_layer <- layers_definition[[i]]
      
      if (is.numeric(current_layer)) {
        
        layers_definition[[i]] <- dense(units = current_layer)
        
      } else {
        
        if (!is.list(current_layer) || is.null(current_layer$type)) {
          
          stop("Malformed layer definition.")
          
        }
        
      }
      
    } 
    
  }
  
  return(layers_definition)
  
}
