add_trainable_model <- function(flow, 
                                using = NULL,
                                output_template = get_problem_info(problem = output)$outputs[1], 
                                inputs = list(),
                                output, 
                                subset = NULL,
                                verbose = TRUE) {
  
  suppressPackageStartupMessages(require(igraph))
  
  # Basic checks
  stopifnot(inherits(flow, "DLflow"))
  stopifnot(file.exists(output_template))
  stopifnot(is.list(using))
  stopifnot(!is.null(using$vol_layers_pattern))
  stopifnot(!is.null(using$last_hidden_layers))
  stopifnot(!is.null(using$units) | !is.null(using$output_width))
  
  if (is.null(using$units))
    using$units <- using$output_width ^ 3
  
  # Parameters from the output template
  if (verbose)
    cat("Preparing output...\n")
  output_info <- analyze_output(output_template)
  
  if (!is.null(subset) & is.list(subset)) {
    
    output_info <- do.call(subset_problem, args = list(info = output_info, 
                                                       subset_classes = subset$subset_classes,
                                                       unify_classes = subset$unify_classes))
    
  }
  
  
  # Definition of the last layer
  last_layer_info <- output_info %>% define_last_layer(units = using$units, 
                                                       force_categorical = TRUE, 
                                                       multioutput = TRUE, 
                                                       hidden_layers = using$last_hidden_layers)
  
  # Remaining parameters to be computed
  vol_layers <- rep(list(using$vol_layers_pattern), times = length(inputs))
  params <- using
  params$vol_layers <- vol_layers
  params$last_layer_info <- last_layer_info
  
  # Configuration of the model
  if (verbose)
    cat("Preparing model configuration...\n")
  config <- do.call(define_config, args = params)
  
  # Model creation
  if (verbose)
    cat("Creating model...\n")
  model <- config %>% create_model_from_config()
  
  # Store the model in the flow
  if (verbose)
    cat("Storing model...\n")
  flow %>% add_process(proc = model, inputs = inputs, output = output, trained = FALSE)
  
  return(invisible(flow))
  
}
