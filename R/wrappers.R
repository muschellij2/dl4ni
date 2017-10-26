##%######################################################%##
#                                                          #
####                      Wrappers                      ####
#                                                          #
##%######################################################%##


#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param units    (name) PARAM_DESCRIPTION
#' @param ...      (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
dense <- function(units, ...) {
  
  list(type = "dense", 
       params = list(units = units, ...))
  
}

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param num_classes    (name) PARAM_DESCRIPTION
#' @param units          (name) PARAM_DESCRIPTION
#' @param ...            (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
categorical <- function(num_classes, units, ...) {
  
  list(type = "categorical", 
       params = list(num_classes = num_classes,
                     units = units, ...))
  
}

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param num_values    (name) PARAM_DESCRIPTION
#' @param units         (name) PARAM_DESCRIPTION
#' @param ...           (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
multivalued <- function(num_values, units, ...) {
  
  list(type = "multivalued", 
       params = list(num_values = num_values,
                     units = units, ...))
  
}

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param units    (name) PARAM_DESCRIPTION
#' @param ...      (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
regression <- function(units, ...) {
  
  list(type = "regression", 
       params = list(units = units, ...))
  
}


#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param filters        (name) PARAM_DESCRIPTION
#' @param kernel_size    (name) PARAM_DESCRIPTION
#' @param ...            (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
conv3d <- function(filters, kernel_size, ...) {
  
  list(type = "conv3d",
       params = list(filters = filters,
                     kernel_size = kernel_size, ...))
  
}

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param ...    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
resnet <- function(...) {
  
  list(type = "resnet",
       params = list(...))
  
}

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param ...    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
clf <- function(...) {
  
  list(type = "clf",
       params = list(...))
  
}
