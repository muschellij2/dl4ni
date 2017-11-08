#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param flow       (name) PARAM_DESCRIPTION
#' @param outputs    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#' 
reset_flow <- function(flow, outputs = "all") {
  
  # Basic input checks
  stopifnot(inherits(flow, "DLflow"))
  
  # Actual list of outputs
  if ("all" %in% outputs) {
    
    outputs <- flow$outputs
    
  } else {
    
    outputs <- intersect(outputs, flow$outputs)
    
  }
  
  # Substitute the model by its scheme
  for (output in outputs) {
    
    if (inherits(flow$processes[[output]], "DLmodel")) {
      
      # flow$processes[[output]] <- NULL
      flow$processes[[output]] <- flow$schemes[[output]]
      flow$trained[[output]] <- FALSE
      
    }
    
  }

  return(invisible(flow))
  
}
