#' @title Continuous Learning of Features Block
#' @description This function composes an object with a CLF block
#' 
#' @param object             (keras object) The object where to append the block.
#' @param hidden_layers      (list) List of layers, with types and corresponding parameters.
#' @param all                (logical) Use CLF technique in all hidden layers?, Default: FALSE
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
                      all = FALSE) {
  
  require(keras)
  
  # Add layers to the input object, depending if CLF must be applied in all layers or not.
  if (all) {

    output <- object %>% 
      add_layers(layers_definition = hidden_layers, 
                 clf = TRUE)
    
  } else {
    
    output <- object %>% 
      add_layers(layers_definition = hidden_layers)
    
    if (is.list(object)) {
      
      for (index in seq_along(object)) {
        
        output[[index]] <- layer_concatenate(c(object[[index]], output[[index]]))
        
      }
      
    } else {
      
      output <- layer_concatenate(c(object, output))
      
    }
    
    
  }
  
  return(output)
  
}
