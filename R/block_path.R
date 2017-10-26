#' Create Independent Paths
#' @description This function appends a set of independent paths with the same architecture to a keras object.
#'
#' @param object             (keras object) The object where to append the block.
#' @param hidden_layers      (list) List of layers, with types and corresponding parameters.
#' @param hidden_activation  (character) Activation function to use in the hidden layers of the block. 
#' @param hidden_dropout     (numeric in [0, 1]) The dropout to use in the hidden layers.
#' @param num_paths          (integer) Number of paths to generate with the same topology.
#' @param concatenate        (logical) Default = \code{FALSE}. Do we have to concatenate all outputs?
#' @param params             (list) List of parameters to apply, if not listed in the previous ones.
#'
#' @return A list of keras objects (if \code{concatenate == FALSE}) or a single object (if \code{concatenate == TRUE}),
#' representing the \code{num_paths} independent paths.
#' @export
#' 
block_paths <- function(object,
                        hidden_layers = NULL,
                        hidden_activation = "relu",
                        hidden_dropout = 0,
                        num_paths = 2,
                        concatenate = FALSE,
                        params = NULL) {
  
  # Override parameters with those given in params.
  if (!is.null(params)) {
    
    if (length(hidden_layers) == 0) {
      
      if (("hidden_layers" %in% names(params)) && (length(params$hidden_layers) > 0)) 
        hidden_layers <- params$hidden_layers
      
    }
    
    hidden_activation <- ifelse("hidden_activation" %in% names(params), params$hidden_activation, hidden_activation)
    hidden_dropout <- ifelse("hidden_dropout" %in% names(params), params$hidden_dropout, hidden_dropout)
    num_paths <- ifelse("num_paths" %in% names(params), params$num_paths, num_paths)
    concatenate <- ifelse("concatenate" %in% names(params), params$concatenate, concatenate)
    
  }
  
  require(keras)
  
  # Build the independent paths.
  outputs <- list()
  
  # For each output unit...
  for (i in seq(num_paths)) {
    
    outputs[[i]] <- object
    
    # If there are hidden layers, add them to the initial object
    if (length(hidden_layers) > 0) {
      
      outputs[[i]] <- (outputs[[i]]) %>% 
        add_layers(layers_definition = hidden_layers,
                   activation = hidden_activation, 
                   dropout = hidden_dropout)
      
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
