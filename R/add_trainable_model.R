add_trainable_model <- function(flow, 
                                scheme = NULL, 
                                inputs = list(),
                                output, 
                                subset = NULL,
                                verbose = TRUE) {
  
  suppressPackageStartupMessages(require(igraph))
  
  # Basic checks
  stopifnot(inherits(flow, "DLflow"))

  stopifnot(is.list(scheme))
  stopifnot(!is.null(scheme$vol_layers_pattern))
  stopifnot(!is.null(scheme$last_hidden_layers))
  stopifnot(!is.null(scheme$units) | !is.null(scheme$output_width))
  
  if (is.null(scheme$units))
    scheme$units <- scheme$output_width ^ 3
  
  # Remaining parameters to be computed
  vol_layers <- rep(list(scheme$vol_layers_pattern), times = length(inputs))
  params <- scheme
  params$vol_layers <- vol_layers
  params$subset <- subset
  
  class(params) <- c("DLscheme", class(params))
  
  # Store the model in the flow
  if (verbose)
    cat("Storing configuration\n")
  flow %>% add_process(proc = params, inputs = inputs, output = output, trained = FALSE)
  
  return(invisible(flow))
  
}
