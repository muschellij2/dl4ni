#' Categorical Block
#' @description This function is used to append a multivalued block to a previously existing object.
#'
#' @param object             (keras object) The object where to append the block.
#' @param hidden_layers      (list) List of layers, with types and corresponding parameters.
#' @param hidden_activation  (character) Activation function to use in the hidden layers of the block. 
#' @param hidden_dropout     (numeric in [0, 1]) The dropout to use in the hidden layers.
#' @param num_classes        (integer) The number of classes in the output of the block.
#' @param units              (integer) Number of output units of the block.
#' @param params             (list) List of parameters to apply, if not listed in the previous ones.
#'
#' @details 
#' A categorical block consists on \code{units} different paths, with the common input \code{object}, 
#' each path is composed by a set of hidden layers (all with the same architecture defined in the 
#' arguments of this function), and ended by a dense layer with \code{num_classes} units and "softmax"
#' activation.
#' 
#' The final output is the concatenation of these \code{units} independent paths.
#' 
#' @return The object composed with the categorical block.
#' @export
#'
block_categorical <- function(object, 
                              hidden_layers = NULL,
                              hidden_activation = "relu",
                              hidden_dropout = 0,
                              num_classes = 2,
                              units = 1,
                              params = NULL) {
  
  if (is.null(params)) {
    
    params <- list(num_paths = units)
    
  } else {
    
    params$num_paths <- ifelse("units" %in% names(params), params$units, units)
    
  }
  
  if ("num_classes" %in% names(params)) num_classes <- params$num_classes
  
  # Build the independent paths.
  outputs <- object %>% block_paths(hidden_layers = hidden_layers, 
                                    hidden_activation = hidden_activation, 
                                    hidden_dropout = hidden_dropout, 
                                    num_paths = params$num_paths, 
                                    params = params)
  
  
  # For each independent path, add the output representation
  for (i in seq_along(outputs)) {
    
    # We add a dense layer with num_classes units and softmax activation
    outputs[[i]] <- (outputs[[i]]) %>% 
      keras::layer_dense(units = num_classes, activation = "softmax") 
    
  }
  
  output <- concatenate_layers(outputs)
  
  return(output)
  
}
