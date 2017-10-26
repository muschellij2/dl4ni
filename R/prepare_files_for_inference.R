#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param file_list    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
prepare_files_for_inference <- function(file_list) {
  
  return(lapply(file_list, read_nifti_to_array))
  
}
