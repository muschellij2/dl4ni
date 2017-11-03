#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param problem_info      (name) PARAM_DESCRIPTION
#' @param subset_classes    (name) PARAM_DESCRIPTION
#' @param unify_classes     (name) PARAM_DESCRIPTION
#' @param use_all           (logical) PARAM_DESCRIPTION, Default: TRUE
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
subset_problem <- function(problem_info, subset_classes, unify_classes = NULL, use_all = TRUE) {
  
  # Basic input checks
  stopifnot(inherits(problem_info, "DLproblem"))
  stopifnot(problem_info$type == "image_labelling")
  
  # New classes
  # Save the previous one in safe place
  problem_info$original_values <- problem_info$values
  problem_info$values <- intersect(subset_classes, problem_info$values)
  num_classes <- length(problem_info$values)
  
  # Extra classes unified:
  if (is.vector(unify_classes) & !is.list(unify_classes)) {
    
    unify_classes <- list(unify_classes)
    
  }
  
  if (!is.null(unify_classes) & (is.list(unify_classes))) {
    
    L <- length(unify_classes)
    
    s <- c()
    t <- c()
    
    for (i in seq(L)) {
      
      num_classes <- num_classes + 1
      s <- c(s, unify_classes[[i]])
      t <- c(t, rep(num_classes, length(unify_classes[[i]])))
      
    }
    
    extra_classes <- list(source = s, target = t)
    
  } else {
    
    L <- 0
    extra_classes <- list(source = c(), target = c())
    
  }
  
  # Append a dummy class only if use_all == TRUE
  if (use_all) {
    
    problem_info$values <- c(problem_info$values, max(problem_info$values) + seq(1, L + 1))
    problem_info$remap_classes <- list(source = c(subset_classes, extra_classes$source),
                                       target = c(seq_along(subset_classes), extra_classes$target),
                                       remaining = num_classes + 1)
    
  } else {
    
    if (L > 0) 
      problem_info$values <- c(problem_info$values, max(problem_info$values) + seq(1, L))
    
    # Remove extra classes by remapping them to 0
    problem_info$remap_classes <- list(source = c(subset_classes, extra_classes$source),
                                       target = c(seq_along(subset_classes), extra_classes$target),
                                       remaining = 0)
    
    
  }
  
  problem_info$subsetted <- TRUE
  
  return(invisible(problem_info))
  
}
