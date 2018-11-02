#' @title Builds a DLmodel from a Given Configuration
#'
#' @description This function uses a previously defined configuration (\code{DLconfig} object) to create and compile
#' a keras model, stored, with the rest of metadata, in a \code{DLmodel} object.
#'
#' @param config    (\code{DLconfig} object) The configuration to use to build a model.
#'
#' @return The \code{DLmodel} object created according to the given configuration.
#'
#' @export 
#' 
create_model_from_config <- function(config) {
  
  suppressPackageStartupMessages(require(keras))
  
  # Basic check
  stopifnot(inherits(config, "DLconfig"))
  
  result <- DLmodel$new()
  result$log("INFO", message = "Model initialiazed.")
  
  if (config$path[1] %in% c("both", "features")) {
    
    result$log("INFO", message = "Adding feature layers.")
    
    # Add layers corresponding to the features input.
    input_features <- layer_input(shape = c(config$num_features))
    
    output_features <- input_features %>% 
      add_layers(layers_definition = config$feature_layers)
    
  }
  
  result$log("INFO", message = "Adding volume layers.")
  
  # Check number of input images
  num_vol_inputs <- length(config$vol_layers)
  
  # Initialize inputs and outputs of the "volumes" paths.
  vol_inputs <- list()
  vol_outputs <- list()
  
  # Check if input volumes should be concatenated
  if (config$concatenate_vol_inputs) {
    
    total_volumes <- sum(config$num_volumes)
    
    if (config$only_convolutionals) {
      
      vol_inputs[[1]] <- layer_input(shape = c(config$width, config$width, 
                                               config$width, total_volumes))
      
    } else {
      
      vol_inputs[[1]] <- layer_input(shape = c(total_volumes * config$width ^ 3)) 
      
    }
    
    vol_outputs[[1]] <- vol_inputs[[1]]
    
    # Add layers in this path
    vol_outputs[[1]] <- (vol_outputs[[1]]) %>% 
      add_layers(layers_definition = config$vol_layers[[1]]) 
    
  } else {
    
    # For each input image
    for (v_input in seq(num_vol_inputs)) {
      
      # The input is always a vector, with width ^ 3 components for each of the volumes inside the image
      # (there can be 4D images).
      if (config$only_convolutionals) {
        
        vol_inputs[[v_input]] <- layer_input(shape = c(config$width, config$width, 
                                                       config$width, config$num_volumes[v_input])) 
        
      } else {
        
        vol_inputs[[v_input]] <- layer_input(shape = c(config$num_volumes[v_input] * config$width ^ 3)) 
        
      }
      
      vol_outputs[[v_input]] <- vol_inputs[[v_input]]
      
      # Add layers in this path
      vol_outputs[[v_input]] <- (vol_outputs[[v_input]]) %>% 
        add_layers(layers_definition = config$vol_layers[[v_input]]) 
      
    }
    
  }
  
  # According to the path definition, we keep only the features path, 
  # the volumes paths, or both, concatenated.
  output_vol <- concatenate_layers(vol_outputs)
  
  # Define the inputs, according to the config$path.
  inputs <- switch(config$path[1],
                   
                   "volumes" = vol_inputs,
                   
                   "both" = c(input_features, vol_inputs),
                   
                   "features" = input_features)
  
  main_output <- switch(config$path[1],
                        
                        "both"     = layer_concatenate(list(output_features, output_vol)),
                        
                        "features" = output_features,
                        
                        "volumes"  = output_vol)
  
  
  # Add layers in the common part of the model
  if (length(config$common_layers) > 0) {
    
    result$log("INFO", message = "Adding common layers.")
    
    main_output <- main_output %>% 
      add_layers(layers_definition = config$common_layers)
    
  }
  
  # Finalize with convolutional?
  if (config$finalize_with_convolutional) {
    
    # Add a new convolutional layer, forcing its output to have the desired dimensions
    layer_to_add <- list(conv3d(filters = 1, 
                                kernel_size = rep(config$convolutional_kernel_size, 3),
                                force = config$output_width,
                                padding = "same",
                                activation = config$vol_activation))
    
    main_output <- main_output %>% 
      add_layers(layers_definition = layer_to_add)
    
  }
  
  # Check that last-but-one layer in case of "only_convolutionals" has the appropriate shape
  if (config$only_convolutionals) {
    
    # Object and expected shapes.
    shape <- main_output %>% object_shape()
    expected_shape <- c(config$output_width, config$output_width, config$output_width)
    
    # When using deconvolutional layers for upsampling, the output is all NAs. This is allowed.
    if (!all(is.na(shape[-4]))) {
      
      # Otherwise, if output shape and expected shapes are not consistent, raise an error.
      if (!all(shape[-4] == expected_shape)) {
        
        message <- paste0("Output shapes are not correct. Expected: (", 
                    paste0(expected_shape, collapse = ", "),
                    ") . Obtained: (", 
                    paste0(shape[-4], collapse = ", "),
                    ")")
        result$log("ERROR", message = message)
        
        stop(message)
        
      }
      
    }
    
  }
  
  # Add last layer
  # If the model is an autoencoder, the last layer should be placed at the end, not here.
  if (config$add_last_layer && !(length(config$decoder_layers) > 0)) {
    
    main_output <- main_output %>% 
      add_layers(layers_definition = list(config$last_layer))
    
  }
  
  
  # If we have decoder layers, define the encoder, decoder and complete model.
  if (length(config$decoder_layers) > 0) {
    
    result$log("INFO", message = "Creating autoencoder.")
    
    # Encoder from the inputs to the current output.
    encoder <- keras_model(inputs = inputs,
                           outputs = main_output)
    
    n_layers_encoder <- length(encoder$layers)
    encoder_output_shape <- (encoder %>% model_shapes())[[n_layers_encoder]]
    
    # Build full model and decoder at the same time
    # Use shared layers
    # Decoder begins with a layer of the same shape as the output of the encoder.
    decoder_input <- layer_input(shape = encoder_output_shape)
    
    second_part_inputs <- list(main_output, decoder_input) 
    full_output <- second_part_inputs %>% 
      add_layers(layers_definition = config$decoder_layers)
    
    # Add last layer
    # If the model is an autoencoder, the last layer is placed here.
    if (config$add_last_layer) {
      
      full_output <- full_output %>% 
        add_layers(layers_definition = list(config$last_layer))
      
    }
    
    model <- keras_model(inputs = inputs, outputs = full_output[[1]])
    decoder <- keras_model(inputs = decoder_input, outputs = full_output[[2]])
    
  } else {
    
    # No autoncoder
    # Create the model
    model <- keras_model(inputs = inputs, outputs = main_output)
    encoder <- NULL
    decoder <- NULL
    
  }
  
  
  # Compile if we are given the optimizer and loss function
  if (!is.null(config$optimizer) && !is.null(config$loss)) {
    
    result$log("INFO", message = "Compiling model.")
    
    # Get the actual optimizer to use, according to the stored configuration
    optimizer <- eval_optimizer(config$optimizer)
    
    # And compile. There is no need to compile encoder and decoder since they are not to be trained.
    model %>% compile(optimizer = optimizer, loss = config$loss)
    
  }
  
  # Store the model and metadata in an object of class DLmodel
  result$update(model = model, 
                width = config$width, 
                best_loss = Inf, 
                encoder = encoder, 
                decoder = decoder,
                hyperparameters = config)
  
  result$log(level = "INFO", message = "Model created.")

  return(result)
  
}
