#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
get_dl4ni_config <- function() {
  
  config <- new.env()
  
  source(file = system.file("config", "config.properties", package = "dl4ni"), local = config)
  
  config <- as.list(config)
  
  class(config) <- "DLconfig"
  
  return(config)
  
}

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param ...    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
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
  
  if (config$only_convolutionals) {
    
    config$path <- "volumes"
    config$category_method <- "simple"
    config$regularize <- NULL
    
  }
  
  if (config$window_width %% 2 == 0) config$window_width <- config$window_width + 1
  config$width <- config$window_width
  
  if (config$output_width %% 2 == 0) config$output_width <- config$output_width + 1

  if (is.null(config$decoder_layers)) {
    
    if (config$categorize_output) {
      
      config$num_classes <- config$last_layer_info$num_classes + 1
      
      if (config$num_classes > 2) {
        
        config$class_balance <- "extensive"
        
      } else {
        
        config$class_balance <- "simple"
        
      }
      
    }
    
  } else {
    
    config$last_decoder_layer <- config$decoder_layers[[length(config$decoder_layers)]]
    config$categorize_output <- config$last_decoder_layer$type == "categorical"
    config$num_classes <- config$last_decoder_layer$params$num_classes
    
  }
  
  if (!is.null(config$regularize) && config$categorize_output && config$category_method == "simple") {
    
    config$regularize <- NULL
    
  }

  
  if (!is.null(config$decoder_layers)) {
    
    config$is_autoencoder <- TRUE
    config$output_width <- config$window_width
    
  }
  
  config$add_last_layer = !(is.null(config$last_layer))
  
  # Memory limit
  config$memory_limit <- config$memory_limit %>% convert_to_bytes()
  
  return(config)
  
}

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param x    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
convert_to_bytes <- function(x) {
  
  ptn <- "(\\d*(.\\d+)*)(.*)"
  num  <- as.numeric(sub(ptn, "\\1", x))
  unit <- sub(ptn, "\\3", x)             
  unit[unit == ""] <- "1" 
  
  mult <- c("1" = 1, "K" = 1024, "M" = 1024^2, "G" = 1024^3)
  
  num * unname(mult[unit])
  
}
