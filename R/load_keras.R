#' @title Load KERAS
#'
#' @description This function is used to load the \code{keras} package with the appropriate backend and system libraries.
#'
#' @export 
#' @import keras
#' 
load_keras <- function() {
  
  # We need this to load the python libraries (libpython2.7.so)
  Sys.setenv(LD_LIBRARY_PATH = "/usr/local/lib/:$LD_LIBRARY_PATH")
  
  # Set backend to tensorflow
  Sys.setenv(KERAS_BACKEND = "tensorflow")
  
  # Require keras
  if (!require(keras))
    stop("Library 'keras' is not available.")
  
  K <- keras::backend()
  
}
