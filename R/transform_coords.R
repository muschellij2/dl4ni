#' @title Tranform Coords Between Images
#' @description This function transform coordinates between images with, possibly, different dimensions.
#' 
#' @param x    (numeric vector) X coordinates in original image.
#' @param y    (numeric vector) Y coordinates in original image.
#' @param z    (numeric vector) Z coordinates in original image.
#' @param Vx   (3d array) Original image.
#' @param Vy   (3d array) Target image.
#' 
#' @return The transformed \code{x}, \code{y} and \code{z}.
#' 
#' @export
#' 
transform_coords <- function(x, y, z, Vx, Vy) {
  
  source_dims <- dim(Vx)
  target_dims <- dim(Vy)
  
  x_ <- (x / source_dims[1]) * target_dims[1]
  y_ <- (y / source_dims[2]) * target_dims[2]
  z_ <- (z / source_dims[3]) * target_dims[3]
  
  return(list(x = x_, y = y_, z = z_))
  
}
