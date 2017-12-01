meshnet <- function(num_filters = 21, num_blocks = 7, dropout = 0.25, out_filters = 0) {
  
  # Dilation rates
  dils <- c(1, 2 ^ (seq(num_blocks - 2) - 1), 1)
  
  # Blocks
  blocks <- list()
  
  for (b in seq(num_blocks)) {
    
    blocks <- c(blocks, 
                conv3d(filters = num_filters, 
                       kernel_size = c(3, 3, 3), 
                       dilation_rate = dils[b], 
                       padding = "same",
                       activation = "relu",
                       batch_normalization = TRUE,
                       dropout = dropout))
    
  }
  
  if (out_filters > 0) {
    
    blocks <- c(blocks,
                conv3d(filters = out_filters, 
                       kernel_size = c(3, 3, 3), 
                       padding = "same",
                       activation = "softmax",
                       batch_normalization = FALSE,
                       dropout = 0))
    
  }
  
  return(blocks)
  
}
