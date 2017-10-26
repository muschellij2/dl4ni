#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param layer_list    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso 
#'  \code{\link[keras]{layer_concatenate}}
#' @export 
#' @importFrom keras layer_concatenate
concatenate_layers <- function(layer_list) {
  
  if (length(layer_list) > 1) {
    
    output <- keras::layer_concatenate(inputs = layer_list)
    
  } else {
    
    output <- layer_list[[1]]
    
  } 
  
  return(output)
  
}
