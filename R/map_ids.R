map_ids <- function(image, remap_classes, invert = FALSE) {

  s <- remap_classes$source
  t <- remap_classes$target
  
  Y <- image
  
  if (!invert) {
    
    for (i in seq_along(s)) {
      
      Y[image == s[i]] <- t[i]
      
    }
    
    extra_classes <- setdiff(unique(as.vector(image[image > 0])), s)
    remaining <- 0
    
    if (!is.null(remap_classes$remaining)) 
      remaining <- remap_classes$remaining
    
    for (k in extra_classes) {
      
      Y[image == k] <- remaining
      
    }
    
  } else {
    
    for (i in seq_along(t)) {
      
      original_indices <- s[which(t == t[i])]
      
      Y[image == t[i]] <- original_indices[1]
      
    }
    
  }
  
  
  return(Y)

}
