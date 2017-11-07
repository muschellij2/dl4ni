#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param image           (name) PARAM_DESCRIPTION
#' @param kernel_sigma    (name) PARAM_DESCRIPTION
#' @param kernel_width    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
smooth_by_gaussian_kernel <- function(image, kernel_sigma, kernel_width) {

  if (kernel_width > 0) {

    kernel_width <- 2 * round(kernel_width / 2) + 1
    kernel <- gaussian_kernel(sigma = kernel_sigma, size = kernel_width)

    new_image <- regularize(image = image, kernel = kernel)

    return(new_image)

  } else {

    return(image)

  }

}

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param sigma         (numeric) PARAM_DESCRIPTION, Default: 1
#' @param dim           (numeric) PARAM_DESCRIPTION, Default: 3
#' @param size          (numeric) PARAM_DESCRIPTION, Default: 3
#' @param normalised    (logical) PARAM_DESCRIPTION, Default: TRUE
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
gaussian_kernel <- function(sigma = 1, 
                            dim = 3, 
                            size = 3, 
                            normalised = TRUE) {
  
  if (dim > length(sigma))
    sigma <- rep(sigma, length.out = dim)
  if (dim > length(size))
    size <- rep(ceiling(size), length.out = dim)
  else size <- ceiling(size)
  
  size <- ifelse(size %% 2 == 1, size, size + 1)
  
  scaleFactors <- max(sigma) / ifelse(sigma == 0, 1, sigma)
  x <- lapply(1:dim, function(i) 1:size[i] - (size[i] + 1)/2)
  centres <- lapply(1:dim, function(i) scaleFactors[i] * x[[i]])
  normFun <- function(a, b) sqrt(a ^ 2 + b ^ 2)
  norms <- Reduce(function(a, b) outer(a, b, FUN = normFun), centres)
  kernel <- array(dnorm(norms, sd = max(sigma)), dim = size)
  
  if (normalised)
    kernel <- kernel / sum(kernel, na.rm = TRUE)
  
  return(kernel)
  
}
