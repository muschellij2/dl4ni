#' @title Save DLmodel
#' @description This function saves all data related to a model in a given path.
#' 
#' @param .model  (\code{DLmodel}) Model to save.
#' @param path    (character) Path where to save the model.
#' @param prefix  (character) Subfolder of the \code{path} where to store all files
#' @param comment (character) Comment to include together with the model parameters, Default: ''
#' 
#' @import jsonlite
#' @import keras
#' 
#' @export
#' 
save_model <- function(.model, 
                       path, 
                       prefix, 
                       comment = "") {
  
  # Check input class
  if ("DLmodel" %in% class(.model)) {
    
    model <- .model$model
    width <- .model$width
    hyperparameters <- .model$hyperparameters
    
  } else {
    
    stop("Model not a DLmodel object.")
    
  }
  
  # Create saving path
  path <- file.path(path, prefix)
  dir.create(path, showWarnings = FALSE)
  
  # Files to be created:
  # 1. JSON with the structure of the model
  # 2. HDF5 with the computed weights
  # 3. TXT with comments
  # 4. RDS with the configuration of the model
  model_definition_file <- file.path(path, paste0(prefix, "_model.json"))
  weights_definition_file <- file.path(path, paste0(prefix, "_weights.hdf5"))
  comment_definition_file <- file.path(path, paste0(prefix, "_comments.txt"))
  hyper_parameters_file <- file.path(path, paste0(prefix, "_hyper.rds"))
  
  # Write the JSON
  model %>% 
    keras::model_to_json() %>% 
    jsonlite::prettify() %>% 
    cat(file = model_definition_file)
  
  # Write the HDF5
  model %>% 
    keras::save_model_weights_hdf5(filepath = weights_definition_file)
  
  # Write the TXT
  if (nchar(comment) > 0) {
    
    cat(comment, file = comment_definition_file, append = TRUE)
    
  }
  
  # Write the RDS
  hyperparameters %>% saveRDS(file = hyper_parameters_file)
  
}


#' @title Load DLmodel
#' @description This function loads different files conforming a \code{DLmodel} object.
#' 
#' @param path    (character) Path where the model is stored.
#' @param prefix  (character) Subfolder of the \code{path} where all files are stored.
#' 
#' @return A \code{DLmodel} object.
#' 
#' @seealso 
#'  \code{\link[keras]{model_from_json}},\code{\link[keras]{load_model_weights_hdf5}},
#'  \code{\link{load_model}}
#'  
#' @export 
#' @importFrom keras model_from_json load_model_weights_hdf5
#' 
load_model <- function(path, prefix) {
  
  # Saving path
  path <- file.path(path, prefix)
  
  if (!file.exists(path)) {
    
    stop("No such directory!")
    
  }
  
  # Read definition
  model_definition_file <- file.path(path, paste0(prefix, "_model.json"))
  weights_definition_file <- file.path(path, paste0(prefix, "_weights.hdf5"))
  comment_definition_file <- file.path(path, paste0(prefix, "_comments.txt"))
  hyper_parameters_file <- file.path(path, paste0(prefix, "_hyper.rds"))
  
  # Load hyperparameters 
  hyperparameters <- readRDS(file = hyper_parameters_file)
  
  # Initialize the model, as created with the former configuration
  output_model <- hyperparameters %>% create_model_from_config()
  
  # # Load model architecture
  # model <- model_definition_file %>% readLines() %>% paste0(collapse = "\n") %>% 
  #   keras::model_from_json()
  
  # Load model weights
  output_model$model %>% keras::load_model_weights_hdf5(filepath = weights_definition_file)
  
  output_model <- list(model = output_model$model,
                       width = hyperparameters$width,
                       hyperparameters = hyperparameters)
  
  class(output_model) <- c("DLmodel")
  
  return(output_model)
  
}
