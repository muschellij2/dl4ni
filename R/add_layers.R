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
      
      input_shape <- object_shape(output)
      
      switch(layer_to_add$type,
             
             "dense" = {
               
               is_volumetric <- length(input_shape) == 4
               
               # in case it comes from a volumetric layer (convolutional),
               # first we have to flatten it.
               if (is_volumetric) {
                 
                 # output <- output %>% layer_flatten()
                 new_layer <- layer_conv3d(filters = layer_to_add$params$units,
                                           kernel_size = input_shape[1:3])
                 
                 
               } else {
                 
                 new_layer <- do.call(layer_dense, args = layer_to_add$params)
                 
               }
               
               output <- output %>% 
                 new_layer 
               
               if (batch_normalization) output <- output %>% layer_batch_normalization()
               
               output <- output %>% layer_activation(activation = activation)
               
               if (dropout > 0) output <- output %>% layer_dropout(rate = dropout)
               
             },
             
             "categorical" = {
               
               params <- layer_to_add$params
               
               output <- output %>% block_categorical(params = params)
               
               if (dropout > 0) output <- output %>% layer_dropout(rate = dropout)
               
             },
             
             "regression" = {
               
               params <- layer_to_add$params
               
               output <- output %>% block_regression(params = params)
               
               if (dropout > 0) output <- output %>% layer_dropout(rate = dropout)
               
             },
             
             
             "multivalued" = {
               
               params <- layer_to_add$params
               
               output <- output %>% block_multivalued(params = params)
               
               if (dropout > 0) output <- output %>% layer_dropout(rate = dropout)
               
             },
             
             "resnet" = {
               
               params <- layer_to_add$params
               
               output <- output %>% block_resnet(params = params)
               
               if (dropout > 0) output <- output %>% layer_dropout(rate = dropout)
               
             },
             
             "clf" = {
               
               params <- layer_to_add$params
               
               output <- output %>% block_clf(params = params)
               
               if (dropout > 0) output <- output %>% layer_dropout(rate = dropout)
               
             },
             
             "unet" = {
               
               params <- layer_to_add$params
               
               output <- output %>% block_unet(params = params)
               
               if (dropout > 0) output <- output %>% layer_dropout(rate = dropout)
               
             },
             
             "downsample" = {
               
               params <- layer_to_add$params
               
               output <- output %>% block_downsample(params = params)
               
               if (dropout > 0) output <- output %>% layer_dropout(rate = dropout)
               
             },
             
             "upsample" = {
               
               params <- layer_to_add$params
               
               output <- output %>% block_upsample(params = params)
               
               if (dropout > 0) output <- output %>% layer_dropout(rate = dropout)
               
             },
             
             "maxpooling" = {
               
               output <- output %>% layer_max_pooling_3d()
               
             },
             
             "upsampling" = {
               
               output <- output %>% layer_upsampling_3d()
               
             },
             
             "conv3d" = {
               
               can_convolutional <- length(input_shape) == 4
               new_width <- 0
               
               if (!is.null(layer_to_add$params$force)) {
                 
                 force <- layer_to_add$params$force
                 
                 if (is.numeric(force)) {
                   
                   force <- as.integer(force)
                   
                 } else {
                   
                   force <- 0L
                   
                 }
                 
                 new_width <- force
                 
                 layer_to_add$params$force <- NULL
                 can_convolutional <- TRUE
                 
               }
               
               new_layer <- do.call(layer_conv_3d, args = layer_to_add$params)
               
               if (can_convolutional) {
                 
                 # Force output width: possibly we're coming from a dense layer
                 if (new_width > 0) {
                   
                   output <- output %>% 
                     layer_dense(units = new_width ^ 3) %>% 
                     layer_reshape(target_shape = c(new_width, new_width, new_width, 1)) 
                   
                 }
                 
                 output <- output %>% 
                   new_layer 
                 
                 if (batch_normalization) output <- output %>% layer_batch_normalization()
                 
                 output <- output %>% layer_activation(activation = activation)
                 
                 if (dropout > 0) output <- output %>% layer_dropout(rate = dropout)
                 
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
