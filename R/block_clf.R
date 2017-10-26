#' @title Continuous Learning of Features Block
#' @description This function composes an object with a CLF block
#' 
#' @param object             (keras object) The object where to append the block.
#' @param hidden_layers      (list) List of layers, with types and corresponding parameters.
#' @param hidden_activation  (character) Activation function to use in the hidden layers of the block. 
#' @param hidden_dropout     (numeric in [0, 1]) The dropout to use in the hidden layers.
#' @param all                (logical) Use CLF technique in all hidden layers?, Default: FALSE
#' @param params             (list) List of parameters to apply, if not listed in the previous ones.
#' 
#' @return The object composed with the Continuous Learning of Features block.
#' 
#' @details 
#' A CLF block consists on a set of hidden layers. If \code{all == TRUE}, avery hidden layer is concatenated 
#' with the input \code{object}, otherwise only the last layer will be concatenated with the input.
#' 
#' @export 
#' 
block_clf <- function(object, 
                      hidden_layers = NULL,
                      hidden_activation = "relu",
                      hidden_dropout = 0,
                      all = FALSE,
                      params = NULL) {
  
  # Override arguments if needed
  if (!is.null(params)) {
    
    if (length(hidden_layers) == 0) {
      
      if (("hidden_layers" %in% names(params)) && (length(params$hidden_layers) > 0)) 
        hidden_layers <- params$hidden_layers
      
    }
    
    all <- ifelse("all" %in% names(params), params$all, all)
    hidden_activation <- ifelse("hidden_activation" %in% names(params), params$hidden_activation, hidden_activation)
    hidden_dropout <- ifelse("hidden_dropout" %in% names(params), params$hidden_dropout, hidden_dropout)
    
  }
  
  require(keras)
  
  # Add layers to the input object, depending if CLF must be applied in all layers or not.
  if (all) {

    output <- object %>% 
      add_layers(layers_definition = hidden_layers, 
                 activation = hidden_activation, 
                 dropout = hidden_dropout,
                 clf = TRUE)
    
  } else {
    
    output <- object %>% 
      add_layers(layers_definition = hidden_layers, 
                 activation = hidden_activation, 
                 dropout = hidden_dropout)
    
    output <- layer_concatenate(c(object, output))
    
  }
  
  return(output)
  
}
