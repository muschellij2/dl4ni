#' @title Object Output Shape
#' 
#' @description This function gets the shape of the output of a layer in a model.
#'
#' @param object   (\code{keras} object) The object to which compute shape.
#'
#' @return A vector with the \code{object} shape
#' @export
#'
object_shape <- function(object) {
  
  parse_shape(object$get_shape())
  
}

#' @title Layer Output Shape
#' 
#' @description This function gets the shape of the output of a layer in a model.
#'
#' @param object   (\code{keras} object) The object to which compute shape.
#'
#' @return A vector with the \code{object} shape
#' @export
#'
layer_shape <- function(object) {
  
  unlist(object$get_output_shape_at(0L)[-1])
  
}


#' @title Layer Output Units
#' 
#' @description This function gets the total number of units of the output of a layer in a model.
#'
#' @param object   (\code{keras} object) The object to which compute shape.
#'
#' @return A vector with the \code{object} shape
#' @export
#'
layer_units <- function(object) {
  
  prod(layer_shape(object))
  
}

model_shapes <- function(model) {
  
  lapply(model$layers, layer_shape)
  
}

model_units <- function(model) {
  
  sapply(model$layers, layer_units)
  
}


parse_shape <- function(shape) {
  
  suppressWarnings({
    
    shape %>% 
      as.character() %>% 
      gsub(x = ., pattern = "(\\(|\\))", replacement = "") %>% 
      strsplit(x = ., split = ",") %>% .[[1]] %>% 
      as.numeric() %>% .[-1]
    
  })
  
}
