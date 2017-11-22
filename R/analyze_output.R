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
    
    y <- as.array(y)
    
    r <- round(y)
    if (length(which(!dplyr::near(as.vector(y), as.vector(r)))) > 0) {
      
      info$type <- "image_regression"
      info$range <- range(as.vector(y))
      
    } else {
      
      info$type <- "image_labelling"
      
      info$values <- sort(unique(as.vector(r[r != 0])))
      info$remap_classes <- list(source = info$values,
                                 target = seq_along(info$values))
      
    }
    
    
  } else {
    
    info$type <- "subject_classification"
    
  }
  
  class(info) <- c("DLproblem", class(info))
  return(invisible(info))
  
}
