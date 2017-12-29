#' Categorical Block
#' @description This function is used to append a multivalued block to a previously existing object.
#'
#' @param object             (keras object) The object where to append the block.
#' @param hidden_layers      (list) List of layers, with types and corresponding parameters.
#' @param num_classes        (integer) The number of classes in the output of the block.
#' @param units              (integer) Number of output units of the block.
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
                              num_classes = 2,
                              units = 1,
                              concatenate = FALSE) {
  
  # Final layer in each path
  finalize_layers <- list(dense(units = num_classes, 
                                activation = "softmax", 
                                batch_normalization = FALSE))
  
  # Build the independent paths.
  outputs <- object %>% block_paths(hidden_layers = hidden_layers, 
                                    num_paths = units,
                                    finalize_layers = finalize_layers,
                                    concatenate = FALSE)
  
  # Concatenate output?
  if (concatenate) {
    
    if (is.list(object)) {
      
      output <- lapply(outputs, concatenate_layers)
      
    } else {
      
      output <- concatenate_layers(outputs)
      
    }
    
    return(output)
    
  } else {
    
    return(outputs)
    
  }
  
}
