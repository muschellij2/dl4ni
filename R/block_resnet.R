#' ResNet Block
#' @description This function is used to append a ResNet block to a previously existing object.
#'
#' @param object             (keras object) The object where to append the block.
#' @param hidden_layers      (list) List of layers, with types and corresponding parameters.
#' @param hidden_activation  (character) Activation function to use in the hidden layers of the block. 
#' @param hidden_dropout     (numeric in [0, 1]) The dropout to use in the hidden layers.
#' @param params             (list) List of parameters to apply, if not listed in the previous ones.
#'
#' @details 
#' A ResNet block consists on a set of hidden layers, and ended by a dense layer with the same number
#' of units as output units in \code{object} and a final layer that adds \code{object} and the last layer.
#' 
#' @return The object composed with the ResNet block.
#' @export
#'
block_resnet <- function(object, 
                         hidden_layers = NULL,
                         hidden_activation = "relu",
                         hidden_dropout = 0,
                         params = NULL) {
  
  # Override parameters
  if (!is.null(params)) {
    
    if (length(hidden_layers) == 0) {
      
      if (("hidden_layers" %in% names(params)) && (length(params$hidden_layers) > 0)) 
        hidden_layers <- params$hidden_layers
      
    }
    
    hidden_activation <- ifelse("hidden_activation" %in% names(params), params$hidden_activation, hidden_activation)
    hidden_dropout <- ifelse("hidden_dropout" %in% names(params), params$hidden_dropout, hidden_dropout)
    
  }
  
  require(keras)
  
  # Number of units as output of object.
  input_units <- object$get_shape()$as_list()[[2]]
  
  # Concatenate hidden layers
  output <- object %>% 
    add_layers(layers_definition = hidden_layers, 
               activation = hidden_activation, 
               dropout = hidden_dropout)
  
  # Add a dense layer
  output <- output %>% 
    layer_dense(units = input_units, activation = hidden_activation)
  
  # Add this last layer to the initial object
  output <- layer_add(c(object, output))
  
  return(output)
  
}
