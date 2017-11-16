create_scheme <- function(...) {
  
  scheme <- new.env()
  
  class(scheme) <- c("DLscheme", class(scheme))
  
  scheme %>% add_attribute(...)
  
  return(scheme)
  
}

add_attribute <- function(scheme, ...) {
  
  # Basic input checks
  stopifnot(inherits(scheme, "DLscheme"))
  
  # What to add, using the same names
  args <- list(...)
  
  var_names <- names(args)
  
  if ("optimizer" %in% var_names) {
    
    optimizer <- var_names$optimizer
    var_names <- setdiff(var_names, "optimizer")
    
    if (!is.character(optimizer)) {
      
      stop("'optimizer' is provided but it must be a string, such as: 'adam', 'sgd', etc.")
      
    }
    
  } 
  
  lapply(var_names, function(vn) {
    
    assign(x = vn, value = args[[vn]], envir = scheme)
    return(invisible())
    
  })

  return(invisible(scheme))
  
}

