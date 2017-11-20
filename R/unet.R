conv_block <- function(object, 
                       num_filters, 
                       kernel_size = c(3, 3, 3),
                       activation = "relu", 
                       batch_normalization = FALSE, 
                       residual = FALSE, 
                       dropout = 0) {
  
  res <- object
  res <- res %>%
    layer_conv_3d(filters = num_filters, kernel_size = c(3, 3, 3), activation = activation, padding = "same")
  
  if (batch_normalization)
    res <- res %>% layer_batch_normalization()
  
  if (dropout > 0)
    res <- res %>% layer_dropout(rate = dropout)
  
  res <- res %>%
    layer_conv_3d(filters = num_filters, kernel_size = c(3, 3, 3), activation = activation, padding = "same")
  
  if (batch_normalization)
    res <- res %>% layer_batch_normalization()
  
  if (residual)
    res <- layer_concatenate(c(object, res))
  
  return(res)
  
}

level_block <- function(object,
                        num_filters,
                        depth,
                        inc = 2,
                        activation = "relu",
                        dropout = 0,
                        batch_normalization = TRUE,
                        use_maxpooling = TRUE,
                        use_upsampling = TRUE,
                        residual = FALSE) {
  
  if (depth > 0) {
    
    res1 <- conv_block(object, 
                       num_filters = num_filters, 
                       activation = activation, 
                       batch_normalization = batch_normalization, 
                       residual = residual, 
                       dropout = dropout)
    
    if (use_maxpooling) {
      
      res <- res1 %>% layer_max_pooling_3d()
      
    } else {
      
      res <- res1 %>% 
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
                               use_maxpooling = use_maxpooling, 
                               use_upsampling = use_upsampling, 
                               residual = residual)
    
    if (use_upsampling) {
      
      res <- res %>% 
        layer_upsampling_3d() %>% 
        layer_conv_3d(filters = num_filters, 
                      kernel_size = c(2, 2, 2), 
                      activation = activation, 
                      padding = "same")
      
    } else {
      
      res <- res %>% 
        layer_conv_3d_transpose(filters = num_filters, 
                                kernel_size = c(3, 3, 3), 
                                strides = c(2, 2, 2), 
                                activation = activation, 
                                padding = "same")
      
    }
    
    res1 <- layer_concatenate(c(res1, res))
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
                       use_maxpooling = TRUE, 
                       use_upsampling = TRUE, 
                       residual = FALSE) {
  
  res <- level_block(object, 
                     num_filters = initial_filters, 
                     depth = depth, 
                     inc = inc_rate, 
                     activation = activation, 
                     dropout = dropout, 
                     batch_normalization = batch_normalization, 
                     use_maxpooling = use_maxpooling, 
                     use_upsampling = use_upsampling, 
                     residual = residual) 
  
  if (out_filters > 0)
    res <- res %>% 
      layer_conv_3d(filters = out_filters, 
                    kernel_size = c(1, 1, 1), 
                    activation = final_activation)
  
  return(res)
  
}
