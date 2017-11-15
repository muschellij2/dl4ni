#' imlab-uiip/keras-segnet
segnet <- function(initial_filters = 2, 
                   kernel_size = c(3, 3, 3), 
                   depth = 5, 
                   mode = c("sampling", "convolutional")) {
  
  layers <- list()
  
  if (length(kernel_size) < 3)
    kernel_size <- rep(kernel_size[1], 3)
  
  # Maxpooling Path
  for (step in seq(depth)) {
    
    num_filters <- 2 ^ (step - 1) * initial_filters
    
    layers <- c(layers, identity(filters = num_filters,
                                 kernel_size = as.integer(kernel_size),
                                 step_length = ifelse(step < 3, 2, 3),
                                 batch_normalization = TRUE,
                                 activation = "relu",
                                 dropout = 0))
    
    if (mode == "sampling") {
      
      layers <- c(layers, list(maxpooling()))
      
    } else {
      
      # Convolutional
      layers <- c(layers, list(maxpooling(mode = "convolutional", num_filters = num_filters)))
    }
    
  }
  
  # Upsampling Path
  for (step in seq(depth, 1, by = -1)) {
    
    num_filters <- 2 ^ (step - 1) * initial_filters
    
    
    if (mode == "sampling") {
      
      layers <- c(layers, list(upsampling()))
      
    } else {
      
      layers <- c(layers, list(upsampling(mode = "convolutional", num_filters = num_filters)))
      
    }
    
    layers <- c(layers, identity(filters = num_filters,
                                 kernel_size = as.integer(kernel_size),
                                 step_length = ifelse(step < 3, 2, 3),
                                 batch_normalization = TRUE,
                                 activation = "relu",
                                 dropout = 0))
    
  }
  
  return(layers)
  
}

identity <- function(filters = 2, kernel_size = c(3L, 3L, 3L), step_length = 1, ...) {
  
  new_layer <- list(type = "conv3d", 
                    params = list(filters = filters,
                                  kernel_size = as.integer(kernel_size),
                                  padding = "same",
                                  ...))
  
  return(rep(list(new_layer), step_length))
  
}
