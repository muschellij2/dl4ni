#' @title Add Layers to a Previous Tensor
#'
#' @description This function is used to add succesive layers to a previous output tensor.
#'
#' @param object                 (\code{keras} object) A tensor (layer) to which add layers.
#' @param layers_definition      (list) List of layers to add, using wrappers such as \code{\link{dense}}, \code{\link{conv3d}}, ... Default: c()
#' @param clf                    (logical) Perform concatenation of input and output layers? (shortcut), Default: FALSE
#'
#' @return The composed object.
#'
#' @export 
#' @import keras
#' 
add_layers <- function(object, 
                       layers_definition = c(), 
                       ...) {
  
  check_args <- function(...) {
    
    passed_args <- list(...)
    
    if ("residual" %in% names(passed_args)) {
      
      residual <- passed_args[["residual"]]
      
    } else {
      
      residual <- FALSE
      
    }
    
    passed_args <- list(...)
    
    if ("channels" %in% names(passed_args)) {
      
      channels <- passed_args[["channels"]]
      
    } else {
      
      channels <- FALSE
      
    }
    
    return(list(residual, channels))
    
  }
  
  require(keras)
  
  # Initialize output
  output <- object
  
  c(clf, channels) %<-% check_args(...)
  
  # If we have layers
  if (length(layers_definition) > 0) {
    
    # Loop over all layers to add
    for (i in seq_along(layers_definition)) {
      
      layer_to_add <- layers_definition[[i]]
      
      # Default specifications for inner layers
      defaults <- get_default_specs()
      
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
      
      # Shape of the current output
      if (is.list(output)) {
        
        input_shape <- object_shape(output[[1]])
        
      } else {
        
        input_shape <- object_shape(output)
        
      }

      # Add layers depending on the actual type of the layer_to_add
      switch(type,
             
             # Dense layer
             "dense" = {
               
               is_volumetric <- length(input_shape) == 4
               
               # In case it comes from a volumetric layer (convolutional),
               # first we have to flatten it.
               if (is_volumetric) {
                 
                 if (channels == "last") {
                   
                   kernel_size = input_shape[1:3]
                   
                 } else {
                   
                   kernel_size = input_shape[2:4]
                   
                 }
                 
                 new_params <- list(filters = params$units,
                                    kernel_size = kernel_size,
                                    activation = this_config$activation)
                 new_layer <- do.call(layer_conv_3d, args = new_params)
                 
               } else {
                 
                 params$activation <- this_config$activation
                 
                 new_layer <- do.call(layer_dense, args = params)
                 
               }
               
               # Add new layer
               output <- output %m>% 
                 new_layer 
               
               # Add additional secondary layers as specified
               if (this_config$batch_normalization) 
                 output <- output %m>% layer_batch_normalization()
               
               # output <- output %>% layer_activation(activation = this_config$activation)
               
               if (this_config$dropout > 0) 
                 output <- output %m>% layer_dropout(rate = this_config$dropout)
               
             },
             
             # A categorical block
             "categorical" = {
               
               params$object <- output
               
               output <- do.call(block_categorical, args = params)
               
             },
             
             # A regression block
             "regression" = {
               
               params$object <- output
               
               output <- do.call(block_regression, args = params)
               
             },
             
             # A multivalued block
             "multivalued" = {
               
               params$object <- output
               
               output <- do.call(block_multivalued, args = params)
               
             },
             
             # A resnet block
             "resnet" = {
               
               output <- output %>% block_resnet(params = params)
               
               if (this_config$dropout > 0) 
                 output <- output %m>% layer_dropout(rate = this_config$dropout)
               
             },
             
             # A CLF block
             "clf" = {
               
               params$object <- output
               
               output <- do.call(block_clf, args = params)
               
             },
             
             # U-Net
             "unet" = {
               
               params$object <- output
               params$batch_normalization <- this_config$batch_normalization
               params$activation <- this_config$activation
               params$dropout <- this_config$dropout
               output <- do.call(block_unet, args = params)
               
             },
             
             "dense_unet" = {
               
               params$object <- output
               params$batch_normalization <- this_config$batch_normalization
               params$activation <- this_config$activation
               params$dropout <- this_config$dropout
               output <- do.call(block_dense_unet, args = params)
               
             },
             
             # Downsample block
             "half" = {
               
               params$object <- output
               params$batch_normalization <- this_config$batch_normalization
               params$activation <- this_config$activation
               params$dropout <- this_config$dropout
               output <- do.call(block_half, args = params)
               
             },
             
             # Upsample block
             "double" = {
               
               params$object <- output
               params$batch_normalization <- this_config$batch_normalization
               params$activation <- this_config$activation
               params$dropout <- this_config$dropout
               output <- do.call(block_double, args = params)
               
             },
             
             # Padding layer
             "pad" = {
               
               # params$object <- output
               
               pad_layer <- do.call(layer_zero_padding_3d, args = params)
               
               output <- output %m>% pad_layer
               
             },
             
             # Maxpooling layer
             "maxpooling" = {
               
               # We can use a maxpooling layer or a convolutional layer with stride 2.
               if (params$mode == "downsampling") {
                 
                 output <- output %m>% layer_max_pooling_3d()
                 
               } else {
                 
                 # Convolutional
                 
                 output <- output %m>% layer_conv_3d(filters = params$num_filters, 
                                                     kernel_size = c(3, 3, 3), 
                                                     strides = c(2, 2, 2), 
                                                     activation = this_config$activation, 
                                                     padding = "same")
                 
               }
               
             },
             
             # Upsampling layer
             "upsampling" = {
               
               # We can use an upsampling layer or a deconvolutional layer with stride 2
               if (params$mode == "upsampling") {
                 
                 output <- output %m>% layer_upsampling_3d()
                 
               } else {
                 
                 # Convolutional
                 
                 output <- output %m>% layer_conv_3d_transpose(filters = params$num_filters, 
                                                               kernel_size = c(3, 3, 3), 
                                                               strides = c(2, 2, 2), 
                                                               activation = this_config$activation, 
                                                               padding = "same")
                 
               }
               
             },
             
             # Convolutional layer
             "conv3d" = {
               
               # We can only add a convolutional layer after another convolutional
               can_convolutional <- length(input_shape) == 4
               new_width <- 0
               
               # Or we can force an intermediate layer with a pre-specified number of units
               # which can be used as a bridge between a dense layer and a convolutional one.
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
               
               # Default padding is "same", useful in models such as SegNet and U-Net
               if (is.null(params$padding)) {
                 
                 params$padding <- "same"
                 
               }
               
               params$activation <- this_config$activation
               
               # The convolutinal layer to add
               new_layer <- do.call(layer_conv_3d, args = params)
               
               if (can_convolutional) {
                 
                 # Force output width: possibly we're coming from a dense layer
                 if (new_width > 0) {
                   
                   output <- output %m>% 
                     layer_dense(units = new_width ^ 3) %m>% 
                     layer_reshape(target_shape = c(new_width, new_width, new_width, 1)) 
                   
                 }
                 
                 # Add the layer
                 output <- output %m>% 
                   new_layer 
                 
                 # Add secondary layers as needed
                 if (this_config$batch_normalization) 
                   output <- output %m>% layer_batch_normalization()
                 
                 if (this_config$dropout > 0) 
                   output <- output %m>% layer_dropout(rate = this_config$dropout)
                 
               } else {
                 
                 stop("Error: cannot add convolutional layer.")
                 
               }
               
             }
      )
      
      if (clf) {
        
        # Concatenate input and output of this block of layers.
        # This may fail, since only denses, convolutional with the same shapes, can be merged
        # Thus, we try to concatenate and if we fail, just make a warning and use the previous output
        
        has_error <- FALSE
        if (is.list(object)) {
          
          tmp_output <- list()
          for (index in seq_along(object)) {
            
            tmp_output[[index]] <- try(layer_concatenate(c(object[[index]], output[[index]])), silent = TRUE)
            has_error <- has_error | inherits(tmp_output[[index]], "try-error")
            
          }
          
        } else {
          
          tmp_output <- try(layer_concatenate(c(object, output)), silent = TRUE)
          has_error <- inherits(tmp_output, "try-error")
          
        }
        
        if (!has_error) {
          
          output <- tmp_output
          
        } else {
          
          warning("Layers could not be concatenated. Using simple output instead.")
          
        }
        
      }
      
    }
    
  } 
  
  return(output)
  
}
