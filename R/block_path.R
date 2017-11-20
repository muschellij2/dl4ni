#' Create Independent Paths
#' @description This function appends a set of independent paths with the same architecture to a keras object.
#'
#' @param object             (keras object) The object where to append the block.
#' @param hidden_layers      (list) List of layers, with types and corresponding parameters.
#' @param num_paths          (integer) Number of paths to generate with the same topology.
#' @param finalize_layers    (list) List of layers, with types and corresponding parameters, to be added at the end of each path. Useful when called from \code{\link{block_categorical}}, \code{\link{block_regression}}, etc.
#' @param concatenate        (logical) Default = \code{FALSE}. Do we have to concatenate all outputs?
#'
#' @return A list of keras objects (if \code{concatenate == FALSE}) or a single object (if \code{concatenate == TRUE}),
#' representing the \code{num_paths} independent paths.
#' @export
#' 
block_paths <- function(object,
                        hidden_layers = NULL,
                        num_paths = 2,
                        finalize_layers = list(),
                        concatenate = FALSE) {
  
  require(keras)
  
  # Build the independent paths.
  outputs <- list()
  
  # For each output unit...
  for (i in seq(num_paths)) {
    
    outputs[[i]] <- object
    
    # If there are hidden layers, add them to the initial object
    if (length(hidden_layers) > 0) {
      
      outputs[[i]] <- (outputs[[i]]) %>% 
        add_layers(layers_definition = hidden_layers)
      
    }
    
    # If there are finalizing layers, add them to the initial object
    # Useful when called from \code{\link{block_categorical}}, \code{\link{block_regression}}, etc.
    if (length(finalize_layers) > 0) {
      
      outputs[[i]] <- (outputs[[i]]) %>% 
        add_layers(layers_definition = finalize_layers)
      
    }

  }
  
  # Concatenate outputs if required
  if (concatenate) {
    
    output <- concatenate_layers(outputs)

    return(output)
    
  } else {
    
    return(outputs)
    
  }
  
}
