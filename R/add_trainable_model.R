add_trainable_model <- function(flow, 
                                using = NULL,
                                output_template, 
                                inputs = list(),
                                output) {
  
  suppressPackageStartupMessages(require(igraph))
  
  # Basic checks
  stopifnot(inherits(flow, "DLflow"))
  stopifnot(file.exists(output_template))
  stopifnot(is.list(using))
  stopifnot(!is.null(using$vol_layers_pattern))
  stopifnot(!is.null(using$last_hidden_layers))
  stopifnot(!is.null(using$units))
  
  # Parameters from the output template
  output_info <- analyze_output(output_template)
  
  # Definition of the last layer
  last_layer_info <- output_info %>% define_last_layer(units = using$units, 
                                                       force_categorical = TRUE, 
                                                       multioutput = TRUE, 
                                                       hidden_layers = using$last_hidden_layers)
  
  # Remaining parameters to be computed
  vol_layers <- rep(using$vol_layers_pattern, length(inputs))
  params <- using
  params$vol_layers <- vol_layers
  params$last_layer_info <- last_layer_info
  
  # Configuration of the model
  config <- do.call(define_config, args = params)
  
  # Model creation
  model <- config %>% create_model_from_config()
  
  flow %>% add_process(proc = model, inputs = inputs, output = output, trained = FALSE)
  
  return(invisible(flow))
  
}
