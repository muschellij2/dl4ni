#' @title Block Downsample
#'
#' @description This function creates a block of convolutional layers with max pooling such that the dimension
#' of the outputs is halved after each layer.
#'
#' @param object             (\code{keras} object) Object to which append this block
#' @param initial_filters    (numeric) Number of filters in the first convolutional layer, Default: 2
#' @param kernel_size        (list or vector) size of the convolution kernels, Default: c(3, 3, 3)
#' @param num_steps          (integer) Number of steps to perform downsampling, Default: 1 if \code{kernel_size} is a vector or its length if it's a list.
#' @param activation         (character) Activation function in the block layers, Default: 'relu'
#' @param params             (list) List of parameters to apply, if not listed in the previous ones.
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
block_downsample <- function(object, 
                             initial_filters = 2, 
                             kernel_size = c(3, 3, 3),
                             num_steps = NULL,
                             activation = "relu",
                             params = NULL) {
  
  require(keras)
  
  # Initialize the output in order to compose
  output <- object
  
  # Override arguments if needed
  if (!is.null(params)) {
    
    initial_filters <- ifelse("initial_filters" %in% names(params), params$initial_filters, initial_filters)
    if (!is.null(params$kernel_size)) {
      
      kernel_size <- params$kernel_size
      
    }
    num_steps <- ifelse("num_steps" %in% names(params), params$num_steps, num_steps)
    activation <- ifelse("activation" %in% names(params), params$activation, activation)
    
  }

  # Basic initialization of arguments
  if (!is.list(kernel_size)) {
    
    kernel_size <- list(kernel_size)
    
  }
  
  if (is.null(num_steps))
    num_steps <- length(kernel_size)
  
  if ((!is.null(num_steps)) && (num_steps > length(kernel_size)))
    kernel_size <- rep(kernel_size, num_steps)
    
  filters <- initial_filters
  
  # In each step, add a convolutional and a max_pooling layers, doubling the
  # number of filters with respect to the previous step
  for (step in seq(num_steps)) {
    
    output <- output %>% 
      layer_conv_3d(filters = filters, 
                    kernel_size = kernel_size[[step]], 
                    padding = "same", 
                    activation = activation) %>% 
      layer_max_pooling_3d()
    
    filters <- filters * 2
    
  }
  
  # Return the composed object
  return(output)
  
}

#' @title Block Upsample
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
block_upsample <- function(object, 
                           initial_filters = NULL, 
                           kernel_size = c(3, 3, 3),
                           num_steps = NULL,
                           activation = "relu", 
                           params = NULL) {
  
  require(keras)
  
  # Initialize the composed object
  output <- object
  
  # Override arguments if needed
  if (!is.null(params)) {
    
    initial_filters <- ifelse("initial_filters" %in% names(params), params$initial_filters, initial_filters)
    if (!is.null(params$kernel_size)) {
      
      kernel_size <- params$kernel_size
      
    }
    num_steps <- ifelse("num_steps" %in% names(params), params$num_steps, num_steps)
    activation <- ifelse("activation" %in% names(params), params$activation, activation)
    
  }
  
  # Initialization of arguments
  if (!is.list(kernel_size)) {
    
    kernel_size <- list(kernel_size)
    
  }
  
  if (is.null(num_steps))
    num_steps <- length(kernel_size)
  
  if ((!is.null(num_steps)) && (num_steps > length(kernel_size)))
    kernel_size <- rep(kernel_size, num_steps)
  
  if (is.null(initial_filters))
    initial_filters <- 2 ^ num_steps
  
  filters <- initial_filters
  
  # In each step, add convolutional and upsampling layers, halving the number
  # of filters in each step.
  for (step in seq(num_steps)) {
    
    output <- output %>% 
      layer_conv_3d_transpose(filters = filters, 
                    kernel_size = kernel_size[[step]], 
                    padding = "same", 
                    activation = activation) %>% 
      layer_upsampling_3d()
    
    filters <- filters / 2
    if (filters < 1) filters <- 1
    
  }
  
  # Return the composed object
  return(output)
  
}


#' @title Block U-Net
#'
#' @description This function creates a U-Net block, that is composed by a \code{\link{block_downsample}} followed
#' by a \code{\link{block_upsample}}.
#'
#' @param object             (\code{keras} object) Object used as input
#' @param initial_filters    (integer) Number of initial filters used in the first layer, Default: 2
#' @param num_down_steps     (integer) Steps for the downsampling path, Default: 3
#' @param num_up_steps       (integer) Steps for the upsampling path, Default: the same value as \code{num_down_steps}
#' @param kernel_size        (list or vector) size of the kernels to use, Default: c(3, 3, 3)
#' @param activation         (character) Activation function in the inner layers, Default: 'relu'
#' @param params             (list) List of parameters to apply, if not listed in the previous ones.
#'
#' @return The composed object.
#'
#' @export 
#' 
block_unet <- function(object, 
                       initial_filters = 2, 
                       num_down_steps = 3, 
                       num_up_steps = num_down_steps,
                       kernel_size = c(3, 3, 3),
                       activation = "relu",
                       params = NULL) {
  
  # Override arguments if needed
  if (!is.null(params)) {
    
    initial_filters <- ifelse("initial_filters" %in% names(params), params$initial_filters, initial_filters)
    if (!is.null(params$kernel_size)) {
      
      kernel_size <- params$kernel_size
      
    }
    num_down_steps <- ifelse("num_down_steps" %in% names(params), params$num_down_steps, num_down_steps)
    num_up_steps <- ifelse("num_up_steps" %in% names(params), params$num_up_steps, num_up_steps)
    activation <- ifelse("activation" %in% names(params), params$activation, activation)
    
  }
  
  # Just concatenate both blocks, a downsampling and an upsampling one.
  output <- object %>% 
    block_downsample(initial_filters = initial_filters,
                     kernel_size = kernel_size,
                     num_steps = num_down_steps,
                     activation = activation) %>% 
    block_upsample(kernel_size = kernel_size,
                   num_steps = num_up_steps,
                   activation = activation)
  
  # Return the composed object
  return(output)
  
}
