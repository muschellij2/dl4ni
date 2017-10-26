#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param object               (name) PARAM_DESCRIPTION
#' @param hidden_layers        (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param hidden_activation    (character) PARAM_DESCRIPTION, Default: 'relu'
#' @param hidden_dropout       (numeric) PARAM_DESCRIPTION, Default: 0
#' @param units                (numeric) PARAM_DESCRIPTION, Default: 1
#' @param output_activation    (character) PARAM_DESCRIPTION, Default: 'linear'
#' @param params               (NULL) PARAM_DESCRIPTION, Default: NULL
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso 
#'  
#' @export 
#' @import keras
block_regression <- function(object, 
                             hidden_layers = NULL,
                             hidden_activation = "relu",
                             hidden_dropout = 0,
                             units = 1,
                             output_activation = "linear",
                             params = NULL) {
  
  if (!is.null(params)) {
    
    if (length(hidden_layers) == 0) {
      
      if (("hidden_layers" %in% names(params)) && (length(params$hidden_layers) > 0)) 
        hidden_layers <- params$hidden_layers
      
    }
    
    hidden_activation <- ifelse("hidden_activation" %in% names(params), params$hidden_activation, hidden_activation)
    hidden_dropout <- ifelse("hidden_dropout" %in% names(params), params$hidden_dropout, hidden_dropout)
    units <- ifelse("units" %in% names(params), params$units, units)
    output_activation <- ifelse("output_activation" %in% names(params), params$output_activation, output_activation)
    
  }
  
  require(keras)
  
  outputs <- list()
  
  for (i in seq(units)) {
    
    outputs[[i]] <- object
    
    if (length(hidden_layers) > 0) {
      
      outputs[[i]] <- (outputs[[i]]) %>% 
        add_layers(layers_definition = hidden_layers,
                   activation = hidden_activation, 
                   dropout = hidden_dropout)
      
    }
    
    outputs[[i]] <- (outputs[[i]]) %>% 
      layer_dense(units = 1, activation = output_activation) 
    
  }
  
  if (units > 1) {
    
    output <- layer_concatenate(inputs = outputs)
    
  } else {
    
    output <- outputs[[1]]
    
  }  
  
  return(output)
  
}
