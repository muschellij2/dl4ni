

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param x        (name) PARAM_DESCRIPTION
#' @param y        (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param text     (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param force    (logical) PARAM_DESCRIPTION, Default: FALSE
#' @param ...      (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso 
#'  \code{\link[neurobase]{ortho2}}

#'  \code{\link[RNifti]{updateNifti}}

#'  \code{\link[RNifti]{updateNifti}}
#' @export 
#' @importFrom neurobase ortho2
#' @importFrom RNifti updateNifti
#' @importFrom RNifti updateNifti
#' @import neurobase
ortho_plot <- function(x, y = NULL, text = NULL, force = FALSE, ...) {
  
  require(neurobase)
  
  if ("niftiImage" %in% class(x))
    class(x) <- "niftiImage"
  
  if (!is.null(y) & ("niftiImage" %in% class(x)))
    y <- RNifti::updateNifti(image = y, template = x)
  
  args <- list(...)
  saving_path <- ifelse("saving_path" %in% names(args), args$saving_path, tempdir())
  saving_prefix <- ifelse("saving_path" %in% names(args), args$saving_prefix, tempfile())
  
  if (!is.null(text)) 
    filename <- file.path(saving_path, 
                          saving_prefix, 
                          paste0("plot_", gsub(pattern = " ", replacement = "_", x = tolower(text))))
                          
  if (!interactive() | force) {
    
    png(filename = filename)
    
  }
                          
  neurobase::ortho2(x = x, y = y,
                    text = text, ...)
  
  if (!interactive() | force) {
    
    dev.off()
    
  }
  
  if (!interactive() | force) 
    return(filename)
  
  return(invisible(NULL))
  
}
