#' @title Block Half
#'
#' @description This function creates a block of convolutional layers with max pooling such that the dimension
#' of the outputs is halved after each block.
#'
#' @param object             (\code{keras} object) Object to which append this block
#' @param initial_filters    (numeric) Number of filters in the first convolutional layer, Default: 2
#' @param kernel_size        (list or vector) size of the convolution kernels, Default: c(3, 3, 3)
#' @param num_steps          (integer) Number of steps to perform downsampling, Default: 1 if \code{kernel_size} is a vector or its length if it's a list.
#' @param activation         (character) Activation function in the block layers, Default: 'relu'
#'
#' @return The composed object.
#'
#' @details In each step, the number of filters is doubled wrt the previous step. Thus, if \code{initial_filters == 2}, the number of filters in the layers in this block is: 2, 4, 8, 16...
#' @seealso block_upsample block_unet
#' @family blocks
#'  
#' @export 
#' @import keras
#' 
block_half <- function(object, 
                       initial_filters = 2, 
                       kernel_size = c(3, 3, 3),
                       num_steps = 1,
                       batch_normalization = FALSE,
                       dropout = 0,
                       use_maxpooling = TRUE,
                       activation = "relu") {
  
  require(keras)
  
  # Initialize the output in order to compose
  output <- object
  
  
  filters <- initial_filters
  
  # In each step, add a convolutional and a max_pooling layers, doubling the
  # number of filters with respect to the previous step
  for (step in seq(num_steps)) {
    
    
    output <- conv_block(output, 
                         num_filters = filters, 
                         kernel_size = kernel_size,
                         activation = activation, 
                         batch_normalization = batch_normalization,
                         dropout = dropout)
    
    if (use_maxpooling) {
      
      output <- output %>% layer_max_pooling_3d()
      
    } else {
      
      output <- output %>% 
        layer_conv_3d(filters = filters, 
                      kernel_size = kernel_size, 
                      strides = c(2, 2, 2), 
                      padding = "same")
      
    }
    
    filters <- filters * 2
    
  }
  
  # Return the composed object
  return(output)
  
}

#' @title Block Double
#'
#' @description This function creates a block of convolutional layers with upsampling such that the dimension
#' of the outputs is doubled after each layer.
#'
#' @param object             (\code{keras} object) Object to which append this block
#' @param initial_filters    (numeric) Number of filters in the first convolutional layer, Default: 2 ^ \code{num_steps}
#' @param kernel_size        (list or vector) size of the convolution kernels, Default: c(3, 3, 3)
#' @param num_steps          (integer) Number of steps to perform downsampling, Default: 1 if \code{kernel_size} is a vector or its length if it's a list.
#' @param activation         (character) Activation function in the block layers, Default: 'relu'
#' @param params             (list) List of parameters to apply, if not listed in the previous ones.
#'
#' @return The composed object.
#'
#' @details In each step, the number of filters is halved wrt the previous step. Thus, if \code{num_steps == 3}, the number of filters in the layers in this block is: 8, 4, 2.
#' 
#' @seealso block_downsample block_unet
#' @family blocks
#'  
#' @export 
#' @import keras
#' 
block_double <- function(object, 
                         initial_filters = NULL, 
                         kernel_size = c(3, 3, 3),
                         num_steps = NULL,
                         batch_normalization = FALSE,
                         dropout = 0,
                         use_upsampling = TRUE,
                         activation = "relu") {
  
  require(keras)
  
  # Initialize the composed object
  output <- object
  
  if (is.null(initial_filters))
    initial_filters <- 2 ^ num_steps
  
  filters <- initial_filters
  
  # In each step, add convolutional and upsampling layers, halving the number
  # of filters in each step.
  for (step in seq(num_steps)) {
    
    
    if (use_upsampling) {
      
      output <- output %>% 
        layer_upsampling_3d() %>% 
        layer_conv_3d(filters = filters, 
                      kernel_size = c(2, 2, 2), 
                      activation = activation, 
                      padding = "same")
      
    } else {
      
      output <- output %>% 
        layer_conv_3d_transpose(filters = filters, 
                                kernel_size = c(3, 3, 3), 
                                strides = c(2, 2, 2), 
                                activation = activation, 
                                padding = "same")
      
    }
    
    output <- conv_block(output, 
                      num_filters = filters, 
                      activation = activation, 
                      batch_normalization = batch_normalization, 
                      dropout = 0)
    
    filters <- filters / 2
    if (filters < 1) filters <- 1
    
  }
  
  # Return the composed object
  return(output)
  
}
