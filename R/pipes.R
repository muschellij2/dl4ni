#' Apply a Function or Add a New Shared Layer to Each Element of a List
#' 
#' @description This function is used in this package to add shared layers.
#'
#' @param lhs   (list of \code{keras} objects or a single keras object) Object(s) used as input to the function or to add a shared layer to.
#' @param rhs   (function or \code{keras} layer) Function to apply over the inputs or definition of a layer to be shared among all inputs.
#'
#' @return A list if the \code{object} was a list or a single value if the \code{object} was just a value.
#' 
#' @details This function is simply a handy wrapper to \code{lhs %>% purrr::map(rhs)}, that works to share layers between different models with \code{keras} functional API.
#' 
#' @import purrr
#' @export
#'
#' @examples
`%m>%` <- function(lhs, rhs) {
  
  # To make it general, consider that we'll always work with lists
  was_list <- TRUE
  if (!is.list(lhs)) {
    
    lhs <- list(lhs)
    was_list <- FALSE
    
  }
  
  # Thus, add shared layers is simply
  output <- try(lhs %>% purrr::map(rhs), silent = TRUE)
  # For base R:
  # output <- try(lapply(seq_along(lhs), function(i) lhs[[i]] %>% rhs), silent = TRUE)
  
  # An error may happen when inputs are of different shape
  if (inherits(output, "try-error")) {
    
    stop("Incompatible inputs.")
    
  }
  
  # Return the output with the same "layout" as the input object
  if (!was_list) output <- output[[1]]
  
  return(output)
  
}
