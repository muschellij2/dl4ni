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
  
  v <- as.vector(V)
  
  return((V - mean(v)) / (sd(v) + .Machine$double.eps))
  
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
  
  v <- as.vector(V)
  
  return((V - mean(v)) / (max(v) - mean(v) + .Machine$double.eps))
  
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
scale_z_masked <- function(V) {
  
  v <- as.vector(V)
  v <- v[v != 0]
  
  return((V - mean(v)) / (sd(v) + .Machine$double.eps))
  
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
scale_max_masked <- function(V) {
  
  v <- as.vector(V)
  v <- v[v != 0]
  
  return(V / (max(v) + .Machine$double.eps))
  
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
scale_meanmax_masked <- function(V) {
  
  v <- as.vector(V)
  v <- v[v != 0]
  
  return((V - mean(v)) / (max(v) - mean(v) + .Machine$double.eps))
  
}

