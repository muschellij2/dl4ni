compile_model <- function(.model) {
  
  suppressPackageStartupMessages(require(keras))
  
  # Check input class
  stopifnot(inherits(.model, "DLmodel")) 
  
  model <- .model$get_model()
  config <- .model$get_config()
  
  # Compile if we are given the optimizer and loss function
  if (!is.null(config$optimizer) && !is.null(config$loss)) {
    
    .model$log("INFO", message = "Compiling model.")
    
    # Get the actual optimizer to use, according to the stored configuration
    optimizer <- eval_optimizer(config$optimizer)
    
    # And compile. There is no need to compile encoder and decoder since they are not to be trained.
    model %>% compile(optimizer = optimizer, loss = config$loss)
    
    # Update the DLmodel
    .model$update(model = model)
    
  }
  
}
