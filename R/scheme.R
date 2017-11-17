#' @title Create Model Scheme
#' 
#' @description This function creates an object of class \code{DLscheme} that contains the basic configuration
#' of a \code{DLmodel}
#'
#' @param ...   attributes to add to the scheme, as key, value pairs.
#' 
#' @details 
#' All schemes should provide at least a \code{optimizer} argument. The rest of parameters, if not provided, are
#' set to their default values, from \code{\link{get_dl4ni_config}}.
#' 
#' Another interesting attribute is \code{memory_limit}. The default, "512M" is rather conservative. Please feel
#' free to increase this quantity (it's a character string) to a reasonable amounto of memory inside the limits
#' of your system (RAM or GPU memory, wherever your system is configured to train). The automatic estimation of
#' batch size in the training phase uses this \code{memory_limit} to adjust the batch size that fits in memory,
#' thus increasing it will allow larger batch sizes and faster training phases. In case you overestimate the
#' available memory, the system may become inestable, so use with caution. Reasonable values could be "2G" in
#' older systems (like mine).
#' 
#' We don't need to tell, in the scheme, the number of input volumes in the model. This will be made in the
#' "instantiation" of the model using \code{\link{instantiate_model}}, which will use additional information
#' about the problem to solve in order to build the actual model.
#' 
#' This strategy is most useful when creating a \code{DLflow} with \code{\link{create_flow}}. In this case, we can
#' define a complete flow using just functions and schemes (although pretrained models are supported) and the
#' actual models will be instantiated (built) in the training phase, according to the inputs and outputs provided.
#'
#' @return An object of the class \code{DLscheme}.
#' @export
#'
create_scheme <- function(...) {
  
  # Create new environment where to store all parameters to define a network
  scheme <- new.env()
  
  # Assign class
  class(scheme) <- c("DLscheme", class(scheme))
  
  # Add attributes as needed
  scheme %>% add_attribute(...)
  
  # Return the scheme, invisibly. This enables to concatenate calls to this function using the 
  # pipe %>% .
  return(invisible(scheme))
  
}

#' @title Add an Attribute to a Scheme
#'
#' @description This function adds an attribute to an existing model scheme.
#' 
#' @param scheme   (\code{DLscheme} object) The scheme to add attributes to.
#' @param ...      attributes to add to the scheme, as key, value pairs.
#'
#' @return The updated scheme.
#' @export
#'
add_attribute <- function(scheme, ...) {
  
  # Basic input checks
  stopifnot(inherits(scheme, "DLscheme"))
  
  # What to add, using the same names
  args <- list(...)
  
  var_names <- names(args)
  
  # If we are provided with an optimizer, we have to parse it for future use. This is very
  # useful when loading a pre-trained model, so we can reinstantiate the optimizer in the
  # current session and resume training.
  if ("optimizer" %in% var_names) {
    
    optimizer <- parse_optimizer(args[["optimizer"]])
    args[["optimizer"]] <- optimizer

  } 
  
  # Assign all variables to the "scheme" environment
  lapply(var_names, function(vn) {
    
    assign(x = vn, value = args[[vn]], envir = scheme)
    return(invisible())
    
  })
  
  # Return the scheme, invisibly. This enables to concatenate calls to this function using the 
  # pipe %>% .
  return(invisible(scheme))
  
}

