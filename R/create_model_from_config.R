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
  
  result <- list()
  
  if ("DLconfig" %in% class(config)) {
    
    path <- config$path[1]
    
    result <- with(config, {
      
      input_features <- layer_input(shape = c(num_features))
      
      output_features <- input_features %>% 
        add_layers(layers_definition = feature_layers,
                   batch_normalization = feature_batch_normalization,
                   activation = feature_activation,
                   dropout = feature_dropout,
                   clf = FALSE)
      
      num_vol_inputs <- length(vol_layers)
      
      vol_inputs <- list()
      vol_outputs <- list()
      
      for (v_input in seq(num_vol_inputs)) {
        
        
        vol_inputs[[v_input]] <- layer_input(shape = c(width ^ 3))
        vol_outputs[[v_input]] <- vol_inputs[[v_input]]
        
        if (initialize_with_lstm && all(is.numeric(lstm_units)) && (length(lstm_units) >= 2)) {
          
          lstm_units <- as.integer(lstm_units)
          
          vol_outputs[[v_input]] <- vol_outputs[[v_input]] %>% 
            layer_reshape(target_shape = c(width, width, width)) %>%
            time_distributed(layer_lstm(units = lstm_units[1]), batch_input_shape = list(NULL, width, width, width)) %>%
            layer_lstm(units = lstm_units[2])
          
        }
        
        # print(v_input)
        # print(str(vol_layers[[v_input]]))
        
        vol_outputs[[v_input]] <- (vol_outputs[[v_input]]) %>% 
          add_layers(layers_definition = vol_layers[[v_input]],
                     batch_normalization = vol_batch_normalization,
                     activation = vol_activation,
                     dropout = vol_dropout,
                     clf = FALSE)
        
        
        if (v_input == 1) {
          
          individual_outputs <- vol_outputs[[v_input]]
          
        } else {

          individual_outputs <- layer_concatenate(list(individual_outputs, vol_outputs[[v_input]]))
          
        }
        
      }
      

      output_vol <- individual_outputs
      
      main_output <- switch(path[1],
                            "both"     = layer_concatenate(list(output_features, output_vol)),
                            "features" = output_features,
                            "volumes"  = output_vol)

      main_output <- main_output %>% 
        add_layers(layers_definition = common_layers,
                   batch_normalization = common_batch_normalization,
                   activation = common_activation,
                   dropout = common_dropout,
                   clf = FALSE)
      
      # Finalize with convolutional?
      
      # Add last layer
      if (add_last_layer) {
        
        main_output <- main_output %>% 
          add_layers(layers_definition = list(last_layer),
                     batch_normalization = FALSE,
                     activation = output_activation,
                     dropout = 0,
                     clf = FALSE)
        
      }
      
      if (!is.null(decoder_layers)) {
        
        encoder <- keras_model(inputs = list(input_features, input_vol),
                               outputs = main_output)
        
        decoder_input <- layer_input(shape = c(last_layer$params$units))
        
        decoder_output <- decoder_input %>%
          add_layers(layers_definition = decoder_layers,
                     batch_normalization = FALSE,
                     activation = decoder_activation,
                     dropout = 0,
                     clf = FALSE)
        
        decoder <- keras_model(inputs = decoder_input,
                               outputs = decoder_output)
        
        main_output <- main_output %>% 
          add_layers(layers_definition = decoder_layers,
                     batch_normalization = FALSE,
                     activation = decoder_activation,
                     dropout = 0,
                     clf = FALSE)
        
        model <- keras_model(inputs = c(input_features, vol_inputs),
                             outputs = main_output)
        
      } else {
        
        model <- keras_model(inputs = c(input_features, vol_inputs),
                             outputs = main_output)
        
        encoder <- NULL
        decoder <- NULL
        
        
      }
      
      
      if (!is.null(optimizer) && !is.null(loss))
        model %>% compile(optimizer = optimizer, loss = loss)
      
      result <- list(model = model, 
                     width = width, 
                     best_loss = Inf, 
                     encoder = encoder, 
                     decoder = decoder)
      
      as.environment(result)
      
    })
    
    result$hyperparameters <- config
    
    class(result) <- c(class(result), "DLmodel")
    
    return(result)
    
  } else {
    
    return(NULL)
    
  }
  
}
