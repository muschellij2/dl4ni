#' Encode Inputs using AutoEncoder
#'
#' @param model   (\code{DLmodel}) The model to use
#' @param inputs  (array) Inputs to apply the model on.
#'
#' @return The outputs given by the encoder of the model.
#' @export
#'
encode <- function(model, inputs) {
  
  require(keras)
  
  # Just predict using the encoder, if it exists
  if (("DLmodel" %in% class(model)) && (!is.null(model$encoder)))
    model$encoder %>% predict_on_batch(inputs)
  
}

#' Decode Inputs using AutoEncoder
#'
#' @param model   (\code{DLmodel}) The model to use
#' @param inputs  (array) Inputs to apply the model on.
#'
#' @return The outputs given by the decoder of the model.
#' @export
decode <- function(model, inputs) {
  
  require(keras)
  
  # Just predict using the decoder, if it exists
  if (("DLmodel" %in% class(model)) && (!is.null(model$decoder)))
    model$decoder %>% predict_on_batch(inputs)
  
}
