#' @title Create Volume Layers Definition from Pattern
#'
#' @description This function replicates the given pattern of layers definition according to a problem definition.
#'
#' @param info       (\code{DLproblem} object) Problem defintion as given by \code{\link{get_problem_info}}
#' @param pattern    (list, a set of layer definitions) The pattern of layers to replicate
#'
#' @return A list with as many replications of the pattern as inputs are defined in the \code{info} object.
#'
#' @export 
#' 
create_vol_layers <- function(info, pattern) {
  
  rep(list(pattern), times = info$num_inputs)
  
}
