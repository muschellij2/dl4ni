dense_block <- function(object, 
                        num_units, 
                        activation = "relu", 
                        batch_normalization = FALSE, 
                        residual = FALSE, 
                        dropout = 0) {
  
  res <- object
  res <- res %m>%
    layer_locally_connected_1d(units = num_units, activation = activation)
  
  if (batch_normalization)
    res <- res %m>% layer_batch_normalization()
  
  if (dropout > 0)
    res <- res %m>% layer_dropout(rate = dropout)
  
  res <- res %m>%
    layer_dense(units = num_units, activation = activation)
  
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

dense_level_block <- function(object,
                              num_units,
                              depth,
                              inc = 2,
                              activation = "relu",
                              dropout = 0,
                              batch_normalization = TRUE,
                              residual = FALSE) {
  
  if (depth > 0) {
    
    res1 <- dense_block(object, 
                        num_units = num_units, 
                        activation = activation, 
                        batch_normalization = batch_normalization, 
                        residual = residual, 
                        dropout = dropout)
    
    res <- res1 %m>% layer_locally_connected_1d(filters = 1, strides = inc)
    
    
    res <- res %>% dense_level_block(num_units = as.integer(num_units / inc), 
                                     depth = depth - 1, 
                                     inc = inc, 
                                     activation = activation, 
                                     batch_normalization = batch_normalization, 
                                     dropout = dropout, 
                                     residual = residual)
    
    res <- res %m>% 
      layer_upsampling_1d(size = inc) 
    
    if (is.list(object)) {
      
      for (index in seq_along(object)) {
        
        res1[[index]] <- layer_concatenate(c(res1[[index]], res[[index]])) 
        
      }
      
    } else {
      
      res1 <- layer_concatenate(c(res1, res))
      
    }
    
    res <- dense_block(res1, 
                       num_units = num_units, 
                       activation = activation, 
                       batch_normalization = batch_normalization, 
                       residual = residual, 
                       dropout = 0)
    
  } else {
    
    res <- dense_block(object, 
                       num_units = num_units, 
                       activation = activation, 
                       batch_normalization = batch_normalization, 
                       residual = residual, 
                       dropout = dropout)
    
  }
  
  return(res)
  
}

block_dense_unet <- function(object, 
                             initial_units = 64, 
                             depth = 4, 
                             inc_rate = 2, 
                             activation = "relu", 
                             final_activation = "softmax",
                             dropout = 0.5, 
                             batch_normalization = FALSE, 
                             residual = FALSE) {
  
  res <- dense_level_block(object, 
                           num_units = initial_units, 
                           depth = depth, 
                           inc = inc_rate, 
                           activation = activation, 
                           dropout = dropout, 
                           batch_normalization = batch_normalization, 
                           residual = residual) 
  
  return(res)
  
}
