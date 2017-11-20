#' @title Get Default Configuration
#'
#' @description This function returns the default configuration stored in the \code{config.properties} file.
#'
#' @return An object of class \code{DLconfig} containing the configuration.
#'
#' @export 
#' 
get_dl4ni_config <- function() {
  
  # Create a new environment to store the configuration
  config <- new.env()
  
  # Source the contents of the configuration file into the recently created environment.
  source(file = system.file("config", "config.properties", package = "dl4ni"), local = config)
  
  # Return it as list
  config <- as.list(config)
  
  # Attach the class
  class(config) <- "DLconfig"
  
  return(config)
  
}


get_default_specs <- function() {
  
  defaults <- get_dl4ni_config()[c("common_dropout", "common_batch_normalization", "common_activation")] 
  names(defaults) <- gsub(x = names(defaults), pattern = "common_", replacement = "")
  
  return(defaults)
  
}
