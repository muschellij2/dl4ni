#' @title Names of Layers in a Model
#' 
#' @description This function returns the names of the layers that form a model in keras.
#'
#' @param model   (\code{keras} model) A model for which to obtain layer names.
#'
#' @return A character vector with all layer names in the model.
#' @export
#'
layer_names <- function(model) {
  
  model$layers %>% purrr::map(~.$name) %>% unlist()
  
}
