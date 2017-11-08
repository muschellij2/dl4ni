##%######################################################%##
#                                                          #
####                      Wrappers                      ####
#                                                          #
##%######################################################%##


#' @title Dense Layer
#'
#' @description This function is a wrapper to define a dense layer, which is built in \code{\link{create_model_from_config}}.
#'
#' @param units    (integer) Number of units in the dense layer
#' @param ...      other arguments for the creation of a dense layer.
#'
#' @return A list with \code{type} = "dense" and \code{params} with the rest of parameters for the dense layer.
#'
#' @export 
#' 
dense <- function(units, ...) {
  
  list(type = "dense", 
       params = list(units = units, ...))
  
}

#' @title Categorical Block
#'
#' @description This function is a wrapper to define a categorical block, which is built in \code{\link{create_model_from_config}}.
#'
#' @param num_classes    (integer) Number of classes for the block
#' @param units          (integer) Number of units in the output block
#' @param ...            other arguments for the creation of a categorical block.
#'
#' @return A list with \code{type} = "categorical" and \code{params} with the rest of parameters for the categorical block.
#'
#' @export 
#' 
categorical <- function(num_classes, units, ...) {
  
  list(type = "categorical", 
       params = list(num_classes = num_classes,
                     units = units, ...))
  
}

#' @title Multivalued Block
#'
#' @description This function is a wrapper to define a multivalued block, which is built in \code{\link{create_model_from_config}}.
#'
#' @param num_values     (integer) Number of values for the block
#' @param units          (integer) Number of units in the output block
#' @param ...            other arguments for the creation of a multivalued block.
#'
#' @return A list with \code{type} = "multivalued" and \code{params} with the rest of parameters for the categorical block.
#'
#' @export 
#' 
multivalued <- function(num_values, units, ...) {
  
  list(type = "multivalued", 
       params = list(num_values = num_values,
                     units = units, ...))
  
}

#' @title Regression Block
#'
#' @description This function is a wrapper to define a regression block, which is built in \code{\link{create_model_from_config}}.
#'
#' @param units    (integer) Number of units in the output layer.
#' @param ...      other arguments for the creation of a regression block.
#'
#' @return A list with \code{type} = "regression" and \code{params} with the rest of parameters for the output block.
#'
#' @export 
#' 
regression <- function(units, ...) {
  
  list(type = "regression", 
       params = list(units = units, ...))
  
}


#' @title Convolutional Layer
#'
#' @description This function is a wrapper to define a convolutional layer, which is built in \code{\link{create_model_from_config}}.
#'
#' @param filters        (integer) Number of filters
#' @param kernel_size    (vector) Size of the kernel
#' @param ...            other arguments for the convolutional layer.
#'
#' @return A list with \code{type} = "conv3d" and \code{params} with the rest of parameters for the layer.
#'
#' @export 
#' 
conv3d <- function(filters, kernel_size, ...) {
  
  args <- list(...)
  
  args$filters <- filters
  args$kernel_size <- kernel_size
  
  list(type = "conv3d",
       params = args)
  
}

#' @title ResNet Block
#'
#' @description This function is a wrapper to define a ResNet block.
#'
#' @param ...    arguments for the \code{\link{block_resnet}} function.
#'
#' @return A list with \code{type} = "resnet" and \code{params} with the rest of parameters that define the block.
#'
#' @export 
#' 
resnet <- function(...) {
  
  list(type = "resnet",
       params = list(...))
  
}

#' @title Continuous Learning of Features Block
#'
#' @description This function is a wrapper to define a CLF block.
#'
#' @param ...    arguments for the \code{\link{block_clf}} function.
#'
#' @return A list with \code{type} = "clf" and \code{params} with the rest of parameters that define the block.
#'
#' @export 
#' 
clf <- function(...) {
  
  list(type = "clf",
       params = list(...))
  
}

#' @title Downsampling Convolutional Block
#'
#' @description This function is a wrapper to define a downsampling convolutional block.
#'
#' @param initial_filters    (numeric) Number of filters in the first convolutional layer, Default: 2
#' @param kernel_size        (list or vector) size of the convolution kernels, Default: c(3, 3, 3)
#' @param ...                arguments for the \code{\link{block_downsample}} function.
#'
#' @return A list with \code{type} = "downsample" and \code{params} with the rest of parameters that define the block.
#'
#' @export 
#' 
downsample <- function(initial_filters = 2, kernel_size = c(3, 3, 3)) {
  
  list(type = "downsample",
       params = list(initial_filters = initial_filters, 
                     kernel_size = kernel_size, 
                     ...))
  
}

#' @title Upsampling Convolutional Block
#'
#' @description This function is a wrapper to define a upsampling convolutional block.
#'
#' @param initial_filters    (numeric) Number of filters in the first convolutional layer, Default: 2
#' @param kernel_size        (list or vector) size of the convolution kernels, Default: c(3, 3, 3)
#' @param ...                arguments for the \code{\link{block_downsample}} function.
#'
#' @return A list with \code{type} = "upsample" and \code{params} with the rest of parameters that define the block.
#'
#' @export 
#' 
upsample <- function(initial_filters = 2, kernel_size = c(3, 3, 3)) {
  
  list(type = "upsample",
       params = list(initial_filters = initial_filters, 
                     kernel_size = kernel_size, 
                     ...))
  
}

#' @title U-Net Block
#'
#' @description This function is a wrapper to define a U-Net convolutional block.
#'
#' @param initial_filters    (integer) Number of initial filters used in the first layer, Default: 2
#' @param kernel_size        (list or vector) size of the kernels to use, Default: c(3, 3, 3)
#' @param num_down_steps     (integer) Steps for the downsampling path, Default: 3
#' @param ...                arguments for the \code{\link{block_downsample}} function.
#'
#' @return A list with \code{type} = "unet" and \code{params} with the rest of parameters that define the block.
#'
#' @export 
#' 
unet <- function(initial_filters = 2, kernel_size = c(3, 3, 3), num_down_steps = 3) {
  
  list(type = "unet",
       params = list(initial_filters = initial_filters, 
                     kernel_size = kernel_size, 
                     num_down_steps = num_down_steps,
                     ...))
  
}
