#' imlab-uiip/keras-segnet
segnet <- function(initial_filters = 2, kernel_size = c(3, 3, 3), num_steps = 5) {
  
  layers <- list()
  
  if (length(kernel_size) < 3)
    kernel_size <- rep(kernel_size[1], 3)
  
  # Maxpooling Path
  for (step in seq(num_steps)) {
    
    num_filters <- 2 ^ (step - 1) * initial_filters
    
    layers <- c(layers, identity(filters = num_filters,
                                 kernel_size = as.integer(kernel_size),
                                 step_length = ifelse(step < 3, 2, 3)))
    
    layers <- c(layers, list(maxpooling()))
    
  }
  
  # Maxpooling Path
  for (step in seq(num_steps, 1, by = -1)) {
    
    num_filters <- 2 ^ (step - 1) * initial_filters
    
    layers <- c(layers, list(upsampling()))
    
    layers <- c(layers, identity(filters = num_filters,
                                 kernel_size = as.integer(kernel_size),
                                 step_length = ifelse(step < 3, 2, 3)))
    
    
  }
  
  return(layers)
  
}

identity <- function(filters = 2, kernel_size = c(3L, 3L, 3L), step_length = 1) {
  
  new_layer <- list(type = "conv3d", 
                    params = list(filters = filters,
                                  kernel_size = as.integer(kernel_size),
                                  padding = "same"))
  
  return(rep(list(new_layer), step_length))
  
}
