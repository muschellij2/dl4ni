#' Read Nifti File
#' @description This function is used to read a NIfTI file and convert the object into an array
#'
#' @param filename (character) The path to the file to be read
#'
#' @return An array with the NIfTI image.
#' @export
#'
read_nifti_to_array <- function(filename) {
  
  # Read using one of the different available packages, in order of read speed.
  # In our benchmarks, RNifti::readNifti is the fastest, with ANTsR and oro.nifti
  # as second and 3rd options.
  
  if (require(RNifti)) {
    
    return(as.array(RNifti::readNifti(filename)))
    
  }
  
  # nocov start
  if (require(ANTsR)) {
    
    return(ANTsR::as.array(ANTsR::antsImageRead(filename)))
    
  }
  
  if (require(neurobase)) {
    
    img <- fast_readnii(filename)
    class(img) <- setdiff(class(img), "array")

    return(as.array(img))
    
  }
  
  if (require(oro.nifti)) {
    
    return(as.array(oro.nifti::readNIfTI(filename, reorient = FALSE)))
    
  }
  
  # nocov end
  
}
