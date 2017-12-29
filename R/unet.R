conv_block <- function(object, 
                       num_filters, 
                       kernel_size = c(3, 3, 3),
                       activation = "relu", 
                       batch_normalization = FALSE, 
                       residual = FALSE, 
                       dropout = 0) {
  
  res <- object
  res <- res %m>%
    layer_conv_3d(filters = num_filters, kernel_size = c(3, 3, 3), activation = activation, padding = "same")
  
  if (batch_normalization)
    res <- res %m>% layer_batch_normalization()
  
  if (dropout > 0)
    res <- res %m>% layer_dropout(rate = dropout)
  
  res <- res %m>%
    layer_conv_3d(filters = num_filters, kernel_size = c(3, 3, 3), activation = activation, padding = "same")
  
  if (batch_normalization)
    res <- res %m>% layer_batch_normalization()
  
  if (residual) {
    
    if (is.list(object)) {
      
      for (index in seq_along(object)) {
        
        res[[index]] <- layer_concatenate(c(object[[index]], res[[index]]))
        
      }
      
    } else {
      
      res <- layer_concatenate(c(object, res))
      
    }
    
  }
  
  return(res)
  
}

level_block <- function(object,
                        num_filters,
                        depth,
                        inc = 2,
                        activation = "relu",
                        dropout = 0,
                        batch_normalization = TRUE,
                        mode = c("sampling", "convolutional"),
                        residual = FALSE) {
  
  if (depth > 0) {
    
    res1 <- conv_block(object, 
                       num_filters = num_filters, 
                       activation = activation, 
                       batch_normalization = batch_normalization, 
                       residual = residual, 
                       dropout = dropout)
    
    if (mode[1] == "sampling") {
      
      res <- res1 %m>% layer_max_pooling_3d()
      
    } else {
      
      res <- res1 %m>% 
        layer_conv_3d(filters = num_filters, 
                      kernel_size = c(3, 3, 3), 
                      strides = c(2, 2, 2), 
                      padding = "same")
      
    }
    
    res <- res %>% level_block(num_filters = as.integer(inc * num_filters), 
                               depth = depth - 1, 
                               inc = inc, 
                               activation = activation, 
                               batch_normalization = batch_normalization, 
                               dropout = dropout, 
                               mode = c("sampling", "convolutional"),
                               residual = residual)
    
    if (mode[1] == "sampling") {
      
      res <- res %m>% 
        layer_upsampling_3d() %m>% 
        layer_conv_3d(filters = num_filters, 
                      kernel_size = c(2, 2, 2), 
                      activation = activation, 
                      padding = "same")
      
    } else {
      
      res <- res %m>% 
        layer_conv_3d_transpose(filters = num_filters, 
                                kernel_size = c(3, 3, 3), 
                                strides = c(2, 2, 2), 
                                activation = activation, 
                                padding = "same")
      
    }
    
    if (is.list(object)) {
      
      for (index in seq_along(object)) {
        
        res1[[index]] <- layer_concatenate(c(res1[[index]], res[[index]])) 
        
      }
      
    } else {
      
      res1 <- layer_concatenate(c(res1, res))
      
    }
    
    res <- conv_block(res1, 
                      num_filters = num_filters, 
                      activation = activation, 
                      batch_normalization = batch_normalization, 
                      residual = residual, 
                      dropout = 0)
    
  } else {
    
    res <- conv_block(object, 
                      num_filters = num_filters, 
                      activation = activation, 
                      batch_normalization = batch_normalization, 
                      residual = residual, 
                      dropout = dropout)
    
  }
  
  return(res)
  
}

block_unet <- function(object, 
                       initial_filters = 64, 
                       out_filters = 0, 
                       depth = 4, 
                       inc_rate = 2, 
                       activation = "relu", 
                       final_activation = "softmax",
                       dropout = 0.5, 
                       batch_normalization = FALSE, 
                       mode = c("sampling", "convolutional"),
                       residual = FALSE) {
  
  res <- level_block(object, 
                     num_filters = initial_filters, 
                     depth = depth, 
                     inc = inc_rate, 
                     activation = activation, 
                     dropout = dropout, 
                     batch_normalization = batch_normalization, 
                     mode = mode,
                     residual = residual) 
  
  if (out_filters > 0)
    res <- res %m>% 
      layer_conv_3d(filters = out_filters, 
                    kernel_size = c(1, 1, 1), 
                    activation = final_activation)
  
  return(res)
  
}
