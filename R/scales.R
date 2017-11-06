scale_z <- function(V) {
  
  return((V - mean(as.vector(V))) / (sd(as.vector(V)) + .Machine$double.eps))
  
}

scale_max <- function(V) {
  
  return(V / (max(as.vector(V)) + .Machine$double.eps))
  
}

scale_meanmax <- function(V) {
  
  return((V - mean(as.vector(V))) / (max(as.vector(V)) - mean(as.vector(V)) + .Machine$double.eps))
  
}
