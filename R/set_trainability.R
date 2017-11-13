#' Set Trainability of Layers in Model
#' @description This function sets the \code{trainable} attribute of a subset of the layers
#' in a model to \code{TRUE} or \code{FALSE}.
#'
#' @param model           (\code{DLmodel}) The model built by \code{\link{create_model_from_config}}.
#' @param layer_names     (character vector) Names of the layers to set their \code{trainable} attribute.
#' @param trainability    (logical vector) New values for the \code{trainable} attribute of the selected layers.
#'
#' @return changes the \code{trainable} attribute in the selected layers in the model, in place.
#' @export
#'
set_trainability <- function(.model, layer_names = NULL, trainability = FALSE) {
  
  # Check input class
  stopifnot(inherits(.model, "DLmodel")) 
  
  # Get layer names
  all_layer_names <- sapply(.model$model$layers, function(s) s$name)
  
  if (is.null(layer_names)) {
    
    layer_names <- all_layer_names
    
  }
  
  # Get the index of selected layers
  idx <- match(layer_names, all_layer_names)
  idx <- idx[!is.na(idx)]
  
  if (length(trainability) == 1) {
    
    trainability <- rep(trainability, length(idx))
    
  }
  
  # Set the trainable attribute of the layers to the desired value
  if (length(idx) > 0) {
    
    for (i in seq_along(idx)) {
      
      .model$model$layers[[idx[i]]]$trainable <- trainability[i]
      
    }
    
  }
  
}
