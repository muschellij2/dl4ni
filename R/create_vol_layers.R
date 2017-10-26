#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param info       (name) PARAM_DESCRIPTION
#' @param pattern    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
create_vol_layers <- function(info, pattern) {
  
  rep(list(pattern), times = info$num_inputs)
  
}
