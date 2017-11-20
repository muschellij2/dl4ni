#' Multivalued Block
#' @description This function is used to append a multivalued block to a previously existing object.
#'
#' @param object             (keras object) The object where to append the block.
#' @param hidden_layers      (list) List of layers, with types and corresponding parameters.
#' @param hidden_activation  (character) Activation function to use in the hidden layers of the block. 
#' @param hidden_dropout     (numeric in [0, 1]) The dropout to use in the hidden layers.
#' @param num_values         (integer) The number of different possible values in the output of the block.
#' @param units              (integer) Number of output units of the block.
#' @param params             (list) List of parameters to apply, if not listed in the previous ones.
#'
#' @details 
#' A multivalued block consists on \code{units} different paths, with the common input \code{object}, 
#' each path is composed by a set of hidden layers (all with the same architecture defined in the 
#' arguments of this function), and ended by a dense layer with \code{num_values} units and "sigmoid"
#' activation followed by another dense layer with one unit and "linear" activation.
#' 
#' The final output is the concatenation of these \code{units} independent paths.
#' 
#' @return The object composed with the multivalued block.
#' @export
#'
block_multivalued <- function(object, 
                              hidden_layers = list(),
                              num_values = 2,
                              units = 1) {

  # Layers to add at the end of each independent path
  finalize_layers <- list(dense(units = num_values, activation = "sigmoid"),
                          dense(units = 1, activation = "linear"))  

  # Build the independent paths.
  output <- object %>% block_paths(hidden_layers = hidden_layers, 
                                    num_paths = units,
                                    finalize_layers = finalize_layers,
                                    concatenate = TRUE)
  
  return(output)
  
}
