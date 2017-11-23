#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param info      (name) PARAM_DESCRIPTION
#' @param output    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso 
#'  \code{\link[neurobase]{readnii}}
#'  \code{\link[dplyr]{near}}
#' @export 
#' @importFrom neurobase readnii
#' @importFrom dplyr near
analyze_output <- function(info = NULL, output) {
  
  if (is.null(info))
    info <- new.env()
  
  # read one of the output files to detect problem type
  y_file <- output[1]
  
  # y <- try(neurobase::readnii(y_file))
  y <- try(read_nifti_to_array(y_file))
  
  if (!inherits(y, "try-error")) {
    
    res <- volume_type(y)
    
    if (res$type == "continuous") {
      
      info$type <- "image_regression"
      info$range <- res$range
      
    } else {
      
      info$type <- "image_labelling"
      
      info$values <- res$values
      info$remap_classes <- res$remap_classes
      
    }
    
  } else {
    
    info$type <- "subject_classification"
    
  }
  
  class(info) <- c("DLproblem", class(info))
  return(invisible(info))
  
}
