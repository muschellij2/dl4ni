#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param object                 (name) PARAM_DESCRIPTION
#' @param layers_definition      (call) PARAM_DESCRIPTION, Default: c()
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
      
      # This should be moved to config.properties
      defaults <- list(batch_normalization = FALSE,
                       activation = "relu",
                       dropout = 0)
      
      type <- layer_to_add$type
      params <- layer_to_add$params
      
      # Overwrite default specification
      this_config <- defaults
      for (nm in names(defaults)) {
        
        if (!is.null(params[[nm]])) {
          
          this_config[[nm]] <- params[[nm]]
          params[[nm]] <- NULL
          
        }
        
      }
      
      input_shape <- object_shape(output)
      
      switch(type,
             
             "dense" = {
               
               is_volumetric <- length(input_shape) == 4
               
               # in case it comes from a volumetric layer (convolutional),
               # first we have to flatten it.
               if (is_volumetric) {
                 
                 new_params <- list(filters = layer_to_add$params$units,
                                    kernel_size = input_shape[1:3])
                 new_layer <- do.call(layer_conv_3d, args = new_params)
                 
               } else {
                 
                 new_layer <- do.call(layer_dense, args = layer_to_add$params)
                 
               }
               
               output <- output %>% 
                 new_layer 
               
               if (this_config$batch_normalization) output <- output %>% layer_batch_normalization()
               
               output <- output %>% layer_activation(activation = this_config$activation)
               
               if (this_config$dropout > 0) output <- output %>% layer_dropout(rate = this_config$dropout)
               
             },
             
             "categorical" = {
               
               output <- output %>% block_categorical(params = params)
               
               if (this_config$dropout > 0) output <- output %>% layer_dropout(rate = this_config$dropout)
               
             },
             
             "regression" = {
               
               output <- output %>% block_regression(params = params)
               
               if (this_config$dropout > 0) output <- output %>% layer_dropout(rate = this_config$dropout)
               
             },
             
             
             "multivalued" = {
               
               output <- output %>% block_multivalued(params = params)
               
               if (this_config$dropout > 0) output <- output %>% layer_dropout(rate = this_config$dropout)
               
             },
             
             "resnet" = {
               
               output <- output %>% block_resnet(params = params)
               
               if (this_config$dropout > 0) output <- output %>% layer_dropout(rate = this_config$dropout)
               
             },
             
             "clf" = {
               
               output <- output %>% block_clf(params = params)
               
               if (this_config$dropout > 0) output <- output %>% layer_dropout(rate = this_config$dropout)
               
             },
             
             "unet" = {
               
               params$object <- output
               output <- do.call(block_unet, args = params)
               
               if (this_config$dropout > 0) output <- output %>% layer_dropout(rate = this_config$dropout)
               
             },
             
             "downsample" = {
               
               output <- output %>% block_downsample(params = params)
               
               if (this_config$dropout > 0) output <- output %>% layer_dropout(rate = this_config$dropout)
               
             },
             
             "upsample" = {
               
               output <- output %>% block_upsample(params = params)
               
               if (this_config$dropout > 0) output <- output %>% layer_dropout(rate = this_config$dropout)
               
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
               
               if (!is.null(params$force)) {
                 
                 force <- params$force
                 
                 if (is.numeric(force)) {
                   
                   force <- as.integer(force)
                   
                 } else {
                   
                   force <- 0L
                   
                 }
                 
                 new_width <- force
                 
                 params$force <- NULL
                 can_convolutional <- TRUE
                 
               }
               
               if (is.null(params$padding)) {
                 
                 params$padding <- "same"
                 
               }
               
               new_layer <- do.call(layer_conv_3d, args = params)
               
               if (can_convolutional) {
                 
                 # Force output width: possibly we're coming from a dense layer
                 if (new_width > 0) {
                   
                   output <- output %>% 
                     layer_dense(units = new_width ^ 3) %>% 
                     layer_reshape(target_shape = c(new_width, new_width, new_width, 1)) 
                   
                 }
                 
                 output <- output %>% 
                   new_layer 
                 
                 if (this_config$batch_normalization) output <- output %>% layer_batch_normalization()
                 
                 output <- output %>% layer_activation(activation = this_config$activation)
                 
                 if (this_config$dropout > 0) output <- output %>% layer_dropout(rate = this_config$dropout)
                 
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
