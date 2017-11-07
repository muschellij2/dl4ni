#' @title Orthographic Plot
#'
#' @description This function renders an orthographic plot of a nifti image.
#'
#' @param x               (\code{nifti} image) Image to plot
#' @param y               (\code{nifti} image) If not NULL, it is an overlay, Default: NULL
#' @param text            (character) Text used as title of the plot, Default: NULL
#' @param interactiveness (logical) If \code{TRUE}, use package \code{papayar} to render an htmlwidget. If \code{FALSE}, use \code{neurobase::ortho2}, Default: TRUE
#' @param force           (logical) Force saving of plot (forces \code{interactiveness = FALSE}), Default: FALSE
#' @param ...             other arguments, such as the \code{saving_path} and \code{saving_prefix} to store in case of \code{force == TRUE}.
#'
#' @seealso 
#'  \code{\link[neurobase]{ortho2}}
#'  \code{\link[RNifti]{updateNifti}}
#'  \code{\link[RNifti]{updateNifti}}
#'  
#' @export 
#' 
#' @importFrom neurobase ortho2
#' @importFrom RNifti updateNifti
#' @importFrom RNifti updateNifti
#' @import neurobase
#' 
ortho_plot <- function(x, y = NULL, text = "", interactiveness = TRUE, force = FALSE, ...) {
  
  require(neurobase)
  
  # Trasform inputs as needed
  if ("niftiImage" %in% class(x))
    class(x) <- "niftiImage"
  
  if (!is.null(y) & ("niftiImage" %in% class(x)))
    y <- RNifti::updateNifti(image = y, template = x)
  
  # Check paths
  args <- list(...)
  saving_path <- ifelse("saving_path" %in% names(args), args$saving_path, tempdir())
  saving_prefix <- ifelse("saving_path" %in% names(args), args$saving_prefix, tempfile())
  
  # Produce interactive output?
  interactiveness <- interactiveness && interactive() && require(papayar)
  interactiveness <- interactiveness && !force
  
  # Output filename
  filename <- file.path(saving_path, 
                        saving_prefix, 
                        paste0("plot_", gsub(pattern = " ", replacement = "_", x = tolower(text))))
  
  # Output PNG file if forced to do so
  if (!interactive() | force) {
    
    png(filename = filename)
    
  }
  
  # Use papayar to render the widget if interactive or plain ortho2 if not interactive
  if (interactiveness) {
    
    if (!is.null(y)) {

      papayar::papaya(list(x, y))
      
    } else {
      
      papayar::papaya(x)
      
    }
    
  } else {
    
    neurobase::ortho2(x = x, y = y,
                      text = text, ...)
    
  }
  
  # Close connection to PNG file and return its filename
  if (!interactive() | force) {
    
    dev.off()
    return(filename)
    
  }
  
  return(invisible(NULL))
  
}
