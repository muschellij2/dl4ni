#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param problem_path    (character) PARAM_DESCRIPTION, Default: 'foo'
#' @param input_path      (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param output_path     (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param num_subjects    (NULL) PARAM_DESCRIPTION, Default: NULL
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso
#'  \code{\link[neurobase]{readnii}}
#'  \code{\link[dplyr]{near}}
#'  \code{\link[utils]{select.list}}
#' @export
#' @importFrom neurobase readnii
#' @importFrom dplyr near
#' @importFrom utils select.list
get_problem_info <- function(problem_path = NULL,
                             input_path = NULL,
                             output_path = NULL,
                             num_subjects = NULL) {
  
  if (is.null(problem_path) & is.null(input_path)) {
    
    stop("At least one of 'problem_path' or 'input_path' must be provided.")
    
  }
  
  info <- list()
  
  if (file.exists(problem_path) ||
      (!is.null(input_path) && file.exists(input_path))) {
    
    if (is.null(input_path) & file.exists(problem_path))
      input_path <- file.path(problem_path, "inputs")
    
    if (!file.exists(input_path))
      stop("Input path: ", input_path, " does not exist.")
    
    if (is.null(output_path) & file.exists(problem_path))
      output_path <- file.path(problem_path, "outputs")
    
    if (is.null(output_path) || !file.exists(output_path))
      output_path <- input_path
    
    # Manage several inputs
    input_dirs <- list.dirs(path = input_path, full.names = TRUE)[-1]
    
    num_inputs <- 1
    
    if (length(input_dirs) > 1 & interactive()) {
      
      choices <- sapply(input_dirs, basename)
      title <- "Select inputs to use:"
      chosen_inputs <- select.list(choices = c(choices, "all"), multiple = TRUE, title = title)
      if ("all" %in% chosen_inputs) chosen_inputs <- choices
      chosen_inputs <- match(chosen_inputs, choices)
      num_inputs <- length(chosen_inputs)
      
    } else {
      
      num_inputs <- length(input_dirs)
      chosen_inputs <- seq(num_inputs)
      
    }
    
    inputs <- list()
    for (dir in seq(num_inputs)) {
      
      name <- basename(input_dirs[chosen_inputs[dir]])
      inputs[[name]] <- list.files(path = input_dirs[chosen_inputs[dir]], full.names = TRUE)
      num_all_subjects <- length(inputs[[dir]])
      
    }
    
    if (is.null(num_subjects)) {
      
      num_subjects <- num_all_subjects
      
    } else {
      
      if (num_subjects < num_all_subjects) {
        
        for (dir in seq(num_inputs)) {
          
          inputs[[dir]] <- inputs[[dir]][seq(num_subjects)]
          
        }
        
      } else {
        
        num_subjects <- num_all_subjects
        
      }
      
    }
    
    # Manage inputs (check number of volumes per input file)
    num_volumes <- c()
    types <- c()
    for (dir in seq(num_inputs)) {
      
      inputs[[dir]] <- inputs[[dir]][seq(num_subjects)]
      img <- read_nifti_to_array(inputs[[dir]][1])
      this_dim <- dim(img)
      nv <- ifelse(length(this_dim) == 3, 1, this_dim[4])
      num_volumes <- c(num_volumes, nv)
      vt <- volume_type(img)
      
      types <- c(types, vt$type)
      
    }
    
    
    # Manage outputs
    # Check if there are more than one possible output
    output_dirs <- list.dirs(path = output_path, full.names = TRUE)[-1]
    if (length(output_dirs) > 0) {
      
      # Select one of them, if we are in an interactive session
      if (interactive()) {
        
        choices <- sapply(output_dirs, basename)
        title <- "Choose a possible output:"
        chosen_output <- utils::select.list(choices = choices, title = title)
        chosen_output <- match(chosen_output, choices)
        output_path <- output_dirs[chosen_output]
        
      } else {
        
        # If in a non-interactive session, take the first one
        output_path <- file.path(output_path, output_dirs[1])
        
      }
      
    }
    
    outputs <- list.files(path = output_path, full.names = TRUE)
    
    outputs <- outputs[seq(num_subjects)]
    
    # read one of the output files to detect problem type
    y_file <- outputs[1]
    
    # y <- try(neurobase::readnii(y_file))
    y <- try(read_nifti_to_array(y_file))
    
    if (!inherits(y, "try-error")) {
      
      res <- volume_type(y)
      
      if (res$type == "continuous") {
        
        info$type <- "image_regression"
        info$range <- res$range
        
      } else {
        
        info$type <- "image_labelling"
        
        info$values <- res$values
        info$remap_classes <- res$remap_classes
        
      }
      
    } else {
      
      info$type <- "subject_classification"
      
    }
    
    info$input_path <- input_path
    info$output_path <- output_path
    info$num_inputs <- num_inputs
    info$num_subjects <- num_subjects
    info$inputs <- inputs
    info$outputs <- outputs
    info$num_volumes <- num_volumes
    info$input_types <- types
    
  } else {
    
    stop("Not a valid path.")
    
  }
  
  info <- as.environment(info)
  class(info) <- c("DLproblem", class(info))
  
  return(info)
  
}
