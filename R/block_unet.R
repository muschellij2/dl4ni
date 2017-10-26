#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param object             (name) PARAM_DESCRIPTION
#' @param initial_filters    (numeric) PARAM_DESCRIPTION, Default: 2
#' @param kernel_size        (call) PARAM_DESCRIPTION, Default: list(c(3, 3, 3))
#' @param num_steps          (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param activation         (character) PARAM_DESCRIPTION, Default: 'relu'
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso 
#'  
#' @export 
#' @import keras
block_downsample <- function(object, 
                             initial_filters = 2, 
                             kernel_size = list(c(3, 3, 3)),
                             num_steps = NULL,
                             activation = "relu") {
  
  require(keras)
  
  output <- object
  
  if (!is.list(kernel_size)) {
    
    kernel_size <- list(kernel_size)
    
  }
  
  if (is.null(num_steps))
    num_steps <- length(kernel_size)
  
  if ((!is.null(num_steps)) && (num_steps > length(kernel_size)))
    kernel_size <- rep(kernel_size, num_steps)
    
  filters <- initial_filters
  
  for (step in seq(num_steps)) {
    
    output <- output %>% 
      layer_conv_3d(filters = filters, 
                    kernel_size = kernel_size[[step]], 
                    padding = "same", 
                    activation = activation) %>% 
      layer_max_pooling_3d()
    
    filters <- filters * 2
    
  }
  
  return(output)
  
}

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param object             (name) PARAM_DESCRIPTION
#' @param initial_filters    (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param kernel_size        (call) PARAM_DESCRIPTION, Default: list(c(3, 3, 3))
#' @param num_steps          (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param activation         (character) PARAM_DESCRIPTION, Default: 'relu'
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso 
#'  
#' @export 
#' @import keras
block_upsample <- function(object, 
                           initial_filters = NULL, 
                           kernel_size = list(c(3, 3, 3)),
                           num_steps = NULL,
                           activation = "relu") {
  
  require(keras)
  
  output <- object
  
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
  
  return(output)
  
}

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param object             (name) PARAM_DESCRIPTION
#' @param initial_filters    (numeric) PARAM_DESCRIPTION, Default: 2
#' @param num_down_steps     (numeric) PARAM_DESCRIPTION, Default: 3
#' @param num_up_steps       (name) PARAM_DESCRIPTION, Default: num_down_steps
#' @param kernel_size        (call) PARAM_DESCRIPTION, Default: c(3, 3, 3)
#' @param activation         (character) PARAM_DESCRIPTION, Default: 'relu'
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
block_unet <- function(object, 
                       initial_filters = 2, 
                       num_down_steps = 3, 
                       num_up_steps = num_down_steps,
                       kernel_size = c(3, 3, 3),
                       activation = "relu") {
  
  output <- object %>% 
    block_downsample(initial_filters = initial_filters,
                     kernel_size = kernel_size,
                     num_steps = num_down_steps,
                     activation = activation) %>% 
    block_upsample(kernel_size = kernel_size,
                   num_steps = num_up_steps,
                   activation = activation)
  
  return(output)
  
}
