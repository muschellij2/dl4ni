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
  
  was_list <- TRUE
  if (!is.list(object)) {
    
    object <- list(object)
    was_list <- FALSE
    
  }
  
  # Build the independent paths.
  outputs <- list()
  
  # For each output unit...
  for (i in seq(num_paths)) {
    
    outputs[[i]] <- object
    
    # If there are hidden layers, add them to the initial object (shared layers)
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
  
  # Transform the output to the desired format
  # Now, it is: first level: num_paths; second level: number of inputs
  # We want it to be first level: num inputs; 2nd level: num_paths
  # So we could concatenate layers and assign outputs to inputs directly
  real_output <- list()
  for (input_index in seq_along(object)) {
    
    real_output[[input_index]] <- list()
    
    for (path_index in seq(num_paths)) {
      
      real_output[[input_index]][[path_index]] <- outputs[[path_index]][[input_index]]
      
    }
    
  }
  
  # Concatenate outputs if required
  if (concatenate) {
    
    output <- lapply(real_output, concatenate_layers)
    
    if (!was_list) output <- output[[1]]

    return(output)
    
  } else {
    
    if (!was_list) real_output <- real_output[[1]]
    
    return(real_output)
    
  }
  
}
