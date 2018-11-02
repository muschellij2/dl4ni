#' Install Needed Python Libraries
#'
#' @details This function is only needed for its side-effect of installing 
#' tensorflow, keras and other python libraries, needed to perform all computations
#' in this package.
#' 
#' @export
#'
install_python_libraries <- function() {
  
  keras::install_keras(extra_packages = c("tensorflow-hub", "pydot"))
  reticulate::virtualenv_install(
    "r-tensorflow", 
    packages = c("git+https://www.github.com/keras-team/keras-contrib.git")
  )
  
}
