#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param image            (name) PARAM_DESCRIPTION
#' @param remap_classes    (name) PARAM_DESCRIPTION
#' @param invert           (logical) PARAM_DESCRIPTION, Default: FALSE
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
map_ids <- function(image, remap_classes, invert = FALSE) {

  s <- remap_classes$source
  t <- remap_classes$target
  
  Y <- 0 * image
  
  if (!invert) {
    
    t_ <- unique(t)
    
    for (i in seq_along(t_)) {

      original_indices <- s[which(t == t_[i])]
      
      Y[image %in% original_indices] <- t_[i]
      
    }
    
    extra_classes <- setdiff(unique(as.vector(image[image > 0])), s)
    remaining <- 0
    
    if (!is.null(remap_classes$remaining)) 
      remaining <- remap_classes$remaining
    
    Y[image %in% extra_classes] <- remaining

  } else {
    
    t_ <- unique(t)
    
    for (i in seq_along(t_)) {
      
      original_indices <- s[which(t == t_[i])]
      
      Y[image == t_[i]] <- original_indices[1]
      
    }
    
  }
  
  return(Y)

}
