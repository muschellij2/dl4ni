parse_optimizer <- function(optimizer) {
  
  if (is.character(optimizer)) {
    
    # The string is the name of the optimizer, with default parameters
    name <- optimizer
    params <- list()
    
  } else {
    
    # We must capture the name of the optimizer, to be able to keep between sessions
    name <- tolower(capture.output(optimizer))
    params <- optimizer$get_config()
    
  }
  
  name <- gsub(x = name, pattern = "optimizer_", replacement = "")
  
  return(list(name = name, params = params))
  
}

eval_optimizer <- function(parsed_optimizer) {
  
  stopifnot(is.list(parsed_optimizer) & !is.null(parsed_optimizer$name))
  
  opt_fun <- paste0("optimizer_", parsed_optimizer$name)
  optimizer <- do.call(what = opt_fun, args = parsed_optimizer$params)
  
  return(optimizer)
  
}
