to_categorical_volume <- function(V) {
  
  unique_labels <- sort(unique(as.vector(V)))
  
  res <- array(0, dim = c(dim(V), length(unique_labels)))
  
  for (i in seq_along(unique_labels)) {
    
    res_ <- res[, , , i]
    res_[V == unique_labels[i]] <- 1
    res[, , , i] <- res_
    
  }
  
  return(res)
  
}
