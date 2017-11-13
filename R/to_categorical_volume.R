#' @title Split a Label Volume in One-hot Encoding
#'
#' @description This function splits a volume (representing a labelling, such as the result of a segmentation) into several binary volumes (concetenated as a 4D volume) such that each individual 3D volume indicates the presence of a class in the image.
#'
#' @param V    (3D array) Input volume
#'
#' @return A 4D volume representing the one-hot encoding of the input 3D volume.
#'
#' @export 
#' 
to_categorical_volume <- function(V, unique_labels = sort(unique(as.vector(V)))) {
  
  # Create the output 4D volume, with the same 3 dimensions as the input, plus an additional
  # dimensions along the number of unique labels computed before.
  res <- array(0, dim = c(dim(V), length(unique_labels)))
  
  # For each unique label, set as 1 all voxels that belong to that label in the output volume
  # in the corresponding dimension
  for (i in seq_along(unique_labels)) {
    
    res_ <- res[, , , i]
    res_[V == unique_labels[i]] <- 1
    res[, , , i] <- res_
    
  }
  
  return(res)
  
}
