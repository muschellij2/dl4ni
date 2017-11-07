#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param V    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
scale_z <- function(V) {
  
  return((V - mean(as.vector(V))) / (sd(as.vector(V)) + .Machine$double.eps))
  
}

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param V    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
scale_max <- function(V) {
  
  return(V / (max(as.vector(V)) + .Machine$double.eps))
  
}

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param V    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
scale_meanmax <- function(V) {
  
  return((V - mean(as.vector(V))) / (max(as.vector(V)) - mean(as.vector(V)) + .Machine$double.eps))
  
}
