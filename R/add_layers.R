#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param object                 (name) PARAM_DESCRIPTION
#' @param layers_definition      (call) PARAM_DESCRIPTION, Default: c()
#' @param batch_normalization    (logical) PARAM_DESCRIPTION, Default: TRUE
#' @param activation             (character) PARAM_DESCRIPTION, Default: 'relu'
#' @param dropout                (numeric) PARAM_DESCRIPTION, Default: 0
#' @param clf                    (logical) PARAM_DESCRIPTION, Default: FALSE
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso 
#'  
#' @export 
#' @import keras
add_layers <- function(object, 
                       layers_definition = c(),
                       batch_normalization = TRUE,
                       activation = "relu",
                       dropout = 0,
                       clf = FALSE) {
  
  require(keras)
  
  output <- object
  
  max_width <- 7
  
  # print(str(layers_definition))
  
  if (!is.list(layers_definition) && is.vector(layers_definition) ) {
    
    tmp <- layers_definition
    
    layers_definition <- list()
    if (length(tmp) > 0) {
      
      for (i in seq_along(tmp)) {
        
        layers_definition[[i]] <- list(type = "dense", params = list(units = tmp[i]))
        
      }
      
    }
    
  }
  
  if (length(layers_definition) > 0) {
    
    for (i in seq_along(layers_definition)) {
      
      layer_to_add <- layers_definition[[i]]
      
      if (is.numeric(layer_to_add)) {
        
        layer_to_add <- list(type = "dense", params = list(units = layer_to_add))
        
      }
      
      switch(layer_to_add$type,
             
             "dense" = {
               
               new_layer <- do.call(layer_dense, args = layer_to_add$params)
               
               output <- output %>% 
                 new_layer 
               
               if (batch_normalization) output <- output %>% layer_batch_normalization()
               
               output <- output %>% layer_activation(activation = activation)
               
               if (dropout > 0) output <- output %>% layer_dropout(rate = dropout)
               
             },
             
             "categorical" = {
               
               params <- layer_to_add$params
               
               output <- output %>% block_categorical(params = params)

             },
             
             "regression" = {
               
               params <- layer_to_add$params
               
               output <- output %>% block_regression(params = params)
               
             },
             
             
             "multivalued" = {
               
               params <- layer_to_add$params
               
               output <- output %>% block_multivalued(params = params)
               
             },

             "resnet" = {
               
               params <- layer_to_add$params
               
               output <- output %>% block_resnet(params = params)
               
             },
             
             "clf" = {
               
               params <- layer_to_add$params
               
               output <- output %>% block_clf(params = params)
               
             },
             
             "conv3d" = {
               
               new_layer <- do.call(layer_conv_3d, args = layer_to_add$params)
               n_units_last_layer <- prod(unlist(output$get_shape()$as_list()[-1]))
               
               can_convolutional <- FALSE
               for (w in seq(max_width)) {
                 
                 filters <- round(n_units_last_layer / (w ^ 3))
                 
                 if (filters * w ^ 3 == n_units_last_layer) {
                   
                   n_filters <- filters
                   partial_width <- w
                   
                   can_convolutional <- TRUE
                   
                 }
                 
               }
               
               if (can_convolutional) {
                 
                 output <- output %>% 
                   layer_reshape(target_shape = c(partial_width, partial_width, partial_width, n_filters)) %>% 
                   new_layer %>% 
                   layer_activation(activation = activation) %>% 
                   layer_flatten()
                 
               } else {
                 
                 stop("Error: cannot add convolutional layer.")
                 
               }
               
             }
      )
      
      if (clf) {
        
        output <- layer_concatenate(c(object, output))
        
      }
      
    }
    
  } 
  
  return(output)
  
  
}
