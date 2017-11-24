volume_type <- function(V) {
  
  y <- as.vector(as.array(V))
  
  r <- round(y)
  
  res <- list()
  
  if (length(which(!dplyr::near(y, r))) > 0) {
    
    res$type <- "continuous"
    res$range <- range(y)
    
  } else {
    
    res$type <- "categorical"
    
    res$values <- sort(unique(r[r != 0]))
    res$remap_classes <- list(source = res$values,
                               target = seq_along(res$values))
    
  }
  
  return(res)
  
}
