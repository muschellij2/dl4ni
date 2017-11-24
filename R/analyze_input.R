#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param info               (name) PARAM_DESCRIPTION
#' @param input_templates    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
#' 
analyze_input <- function(info = NULL, input) {
  
  if (is.null(info))
    info <- new.env()
    
  num_inputs <- length(input)
  num_volumes <- c()
  types <- c()
  
  for (i in seq(num_inputs)) {
    
    img <- read_nifti_to_array(input[[i]][1])
    this_dim <- dim(img)
    nv <- ifelse(length(this_dim) == 3, 1, this_dim[4])
    num_volumes <- c(num_volumes, nv)
    vt <- volume_type(img)
    
    types <- c(types, vt$type)
    
  }
  
  info$num_inputs <- num_inputs
  info$num_volumes <- num_volumes
  info$input_types <- types
  
  class(info) <- c("DLproblem", class(info))
  return(invisible(info))
  
}
