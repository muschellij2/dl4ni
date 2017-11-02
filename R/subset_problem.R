#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param problem_info      (name) PARAM_DESCRIPTION
#' @param subset_classes    (name) PARAM_DESCRIPTION
#' @param use_all           (logical) PARAM_DESCRIPTION, Default: TRUE
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
subset_problem <- function(problem_info, subset_classes, use_all = TRUE) {
  
  # Basic input checks
  stopifnot(inherits(problem_info, "DLproblem"))
  stopifnot(problem_info$type == "image_labelling")
  
  # New classes
  # Save the previous one in safe place
  problem_info$original_values <- problem_info$values
  problem_info$values <- intersect(subset_classes, problem_info$values)
  num_classes <- length(problem_info$values)
  
  # Append a dummy class only if use_all == TRUE
  if (use_all) {
    
    problem_info$values <- c(problem_info$values, max(problem_info$values) + 1)
    problem_info$remap_classes <- list(source = subset_classes,
                                       target = seq_along(subset_classes),
                                       remaining = num_classes + 1)
    
  } else {
    
    # Remove extra classes by remapping them to 0
    problem_info$remap_classes <- list(source = subset_classes,
                                       target = seq_along(subset_classes),
                                       remaining = 0)
    
    
  }
  
  problem_info$subsetted <- TRUE
  
  return(invisible(problem_info))
  
}
