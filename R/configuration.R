#' @title Configuration Definition
#'
#' @description This function is used to define the configuration to create a model.
#'
#' @param ...    attributes to use
#'
#' @return 
#' The configuration for a model, of class \code{DLconfig}. A new model can be created by using 
#' \code{\link{create_model_from_config}} using the output of this function.
#'
#' @details 
#' There are lots of parameters to tweak a model. Inspect the configuration file \code{config.properties} 
#' to know more.
#' 
#' @export 
#' 
define_config <- function(...) {
  
  args <- list(...)
  
  # Default configuration
  config <- get_dl4ni_config()
  
  if ("last_layer_info" %in% names(args)) {
    
    for (field in names(args$last_layer_info)) {
      
      args[[field]] <- args$last_layer_info[[field]]
      
    }

  }
  
  # Overwrite the defaults
  for (field in names(args)) {
    
    config[[field]] <- args[[field]]
    
  }
  
  # Specific logic for when we use only convolutional layers
  if (config$only_convolutionals) {
    
    # Use only "volumes" path, features are not needed
    # Also, no need to smooth the output and the inference can be made faster.
    config$path <- "volumes"
    config$category_method <- "simple"
    config$regularize <- NULL
    
  }
  
  # Specific logic for when using autoencoders
  if (is.null(config$decoder_layers)) {
    
    # General case in which we are not defining an autoencoder.
    if (config$categorize_output) {
      
      # Number of classes of the last layer.
      config$num_classes <- config$last_layer_info$num_classes + 1
      
      # Make class balancing
      if (config$num_classes > 2) {
        
        config$class_balance <- "extensive"
        
      } else {
        
        config$class_balance <- "simple"
        
      }
      
    }
    
  } else {
    
    # Decoder
    config$last_decoder_layer <- config$decoder_layers[[length(config$decoder_layers)]]
    config$categorize_output <- config$last_decoder_layer$type == "categorical"
    config$num_classes <- config$last_decoder_layer$params$num_classes
    
  }
  
  # Specific logic for the inference
  if (!is.null(config$regularize) && config$categorize_output && config$category_method == "simple") {
    
    # No need to smooth the output image if the category_method is "simple"
    config$regularize <- NULL
    
  }

  # Specific logic for when using autoencoders
  if (!is.null(config$decoder_layers)) {
    
    config$is_autoencoder <- TRUE
    config$output_width <- config$width
    
  }
  
  config$add_last_layer <- !(is.null(config$last_layer))
  
  # Memory limit
  config$memory_limit <- config$memory_limit %>% convert_to_bytes()
  
  return(config)
  
}


