#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param config    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#' 
create_model_from_config <- function(config) {
  
  suppressPackageStartupMessages(require(keras))
  
  # Basic check
  stopifnot(inherits(config, "DLconfig"))
  
  result <- list()
  
  input_features <- layer_input(shape = c(config$num_features))
  
  output_features <- input_features %>% 
    add_layers(layers_definition = config$feature_layers,
               clf = FALSE)
  
  num_vol_inputs <- length(config$vol_layers)
  
  vol_inputs <- list()
  vol_outputs <- list()
  
  for (v_input in seq(num_vol_inputs)) {
    
    vol_inputs[[v_input]] <- layer_input(shape = c(config$num_volumes[v_input] * config$width ^ 3)) 
    
    vol_outputs[[v_input]] <- vol_inputs[[v_input]] %>% 
      layer_reshape(target_shape = c(config$width, config$width, config$width, config$num_volumes[v_input])) %>% 
      layer_permute(dims = c(3, 2, 1, 4))
    
    vol_outputs[[v_input]] <- (vol_outputs[[v_input]]) %>% 
      add_layers(layers_definition = config$vol_layers[[v_input]],
                 clf = FALSE) 
    
    shape <- vol_outputs[[v_input]] %>% object_shape()
    
    if (!config$only_convolutionals & length(shape) > 2) {
      
      vol_outputs[[v_input]] <- (vol_outputs[[v_input]]) %>%
        layer_flatten()
      
    }
    
    if (v_input == 1) {
      
      individual_outputs <- vol_outputs[[v_input]]
      
    } else {
      
      individual_outputs <- layer_concatenate(list(individual_outputs, vol_outputs[[v_input]]))
      
    }
    
  }
  
  output_vol <- individual_outputs
  
  main_output <- switch(config$path[1],
                        "both"     = layer_concatenate(list(output_features, output_vol)),
                        "features" = output_features,
                        "volumes"  = output_vol)
  
  
  if (length(config$common_layers) > 0) {
    
    main_output <- main_output %>% 
      add_layers(layers_definition = config$common_layers,
                 clf = FALSE)
    
  }
  
  # Finalize with convolutional?
  if (config$finalize_with_convolutional) {
    
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
    
    shape <- main_output %>% object_shape()
    expected_shape <- c(config$output_width, config$output_width, config$output_width)
    
    if (!all(is.na(shape[-4]))) {
      
      if (!all(shape[-4] == expected_shape))
        stop(paste0("Output shapes are not correct. Expected: (", 
                    paste0(expected_shape, collapse = ", "),
                    ") . Obtained: (", 
                    paste0(shape[-4], collapse = ", "),
                    ")"))
      
    }
    
  }
  
  # Add last layer
  if (config$add_last_layer) {
    
    main_output <- main_output %>% 
      add_layers(layers_definition = list(config$last_layer),
                 clf = FALSE)
    
  }
  
  if (!is.null(config$decoder_layers)) {
    
    encoder <- keras_model(inputs = list(input_features, vol_inputs),
                           outputs = main_output)
    
    decoder_input <- layer_input(shape = c(config$last_layer$params$units))
    
    decoder_output <- decoder_input %>%
      add_layers(layers_definition = config$decoder_layers,
                 clf = FALSE)
    
    decoder <- keras_model(inputs = decoder_input,
                           outputs = decoder_output)
    
    main_output <- main_output %>% 
      add_layers(layers_definition = config$decoder_layers,
                 clf = FALSE)
    
  } else {
    
    encoder <- NULL
    decoder <- NULL
    
    
  }
  
  model <- switch(config$path[1],
                  
                  "volumes" = keras_model(inputs = vol_inputs,
                                          outputs = main_output),
                  
                  "both" = keras_model(inputs = c(input_features, vol_inputs),
                                       outputs = main_output),
                  
                  "features" = keras_model(inputs = input_features,
                                           outputs = main_output) )
  
  
  if (!is.null(config$optimizer) && !is.null(config$loss)) {
    
    # Get the actual optimizer to use, according to the stored configuration
    optimizer <- eval_optimizer(config$optimizer)
    
    # And compile
    model %>% compile(optimizer = optimizer, loss = config$loss)
    
  }
  
  result <- new.env()
  result$model <- model
  result$width <- config$width
  result$best_loss <- Inf
  result$encoder <- encoder
  result$decoder <- decoder
  result$hyperparameters <- config
  
  class(result) <- c("DLmodel", class(result))
  
  return(result)
  
}
