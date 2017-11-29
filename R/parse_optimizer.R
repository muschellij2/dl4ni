#' @title Parse Keras Optimizer
#' 
#' @description This function parses the configuration of the optimizer used to train a model 
#' in order to be able to save and load it.
#'
#' @param optimizer    (character or keras optimizer) Optimizer name (e.g. "adadelta") or an instance of a
#' optimizer (e.g. optimizer_adadelta()).
#'
#' @return A list with the name of the optimizer and its configuration.
#' @export
#'
parse_optimizer <- function(optimizer) {
  
  if (is.character(optimizer)) {
    
    # The string is the name of the optimizer, with default parameters
    name <- gsub(x = optimizer, pattern = "optimizer_", replacement = "")
    
    # Let's check if it's a valid optimizer
    valid_names <- gsub(x = ls(pattern = "optimizer_", name = "package:keras"), 
                        pattern = "optimizer_", 
                        replacement = "")
    
    if (!(name %in% valid_names))
      stop("'optimizer' is not a valid optimizer for Keras.")
    
    params <- list()
    
  } else {
    
    if (is.null(optimizer)) return(NULL)
    
    if (!inherits(optimizer, "keras.optimizers.Optimizer")) {
      
      stop("'optimizer' is not a valid optimizer for Keras.")
      
    }
    
    # We must capture the name of the optimizer, to be able to keep between sessions
    name <- tolower(capture.output(optimizer))
    params <- optimizer$get_config()
    
  }
  
  return(list(name = name, params = params))
  
}



#' @title Evaluate a Parsed Optimizer
#' 
#' @description This function is the complement for \code{\link{parse_optimizer}}, since it reconstructs the original
#' optimizer from its configuration, as needed to compile a model.
#'
#' @param parsed_optimizer   (list) The result of a call to \code{\link{parse_optimizer}}.
#'
#' @return A keras optimizer instance for the current session.
#' @export
#'
eval_optimizer <- function(parsed_optimizer) {
  
  # Basic input checks
  stopifnot(is.list(parsed_optimizer) & !is.null(parsed_optimizer$name))
  
  # Optimizer name
  opt_fun <- paste0("optimizer_", parsed_optimizer$name)
  
  # Instantiation of the optimizer in the current session
  err <- try(
    optimizer <- do.call(what = opt_fun, args = parsed_optimizer$params)
  )
  
  if (inherits(err, "try-error")) {
    
    stop("Not a valid optimizer.")
    
  }
  
  return(optimizer)
  
}
