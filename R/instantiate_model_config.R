instantiate_model_config <- function(scheme, 
                                     info = NULL, 
                                     inputs = NULL, 
                                     outputs = NULL, 
                                     labels_subset = NULL) {
  
  # Basic input checks
  stopifnot(inherits(scheme, "DLscheme"))
  stopifnot(!is.null(info) | (!is.null(inputs) & !is.null(outputs)))
  
  default_config <- get_dl4ni_config()
  scheme_list <- scheme$to_list()
  
  # Overwrite defaults
  for (nm in names(scheme_list)) {
    
    if (!is.null(scheme_list[[nm]]))
      default_config[[nm]] <- scheme_list[[nm]]
    
  }
  
  output_width <- default_config$output_width
  only_convolutionals <- default_config$only_convolutionals
  
  if (!is.null(scheme_list$is_autoencoder) && scheme_list$is_autoencoder & is.null(outputs)) {
    
    outputs <- inputs[[1]]
    
  }
  
  if (is.null(info)) {
    
    # Create info from data provided
    info <- analyze_input(input = inputs) %>% analyze_output(output = outputs)
    
  }
  
  if (!is.null(info)) {
    
    if (!is.null(labels_subset)) {
      
      args <- labels_subset
      args$problem_info <- info
      
      info <- do.call(subset_problem, args = args)
      
    }
    
    to_add <- list(num_inputs = info$num_inputs,
                   num_volumes = info$num_volumes,
                   input_types = info$input_types,
                   remap_classes = info$remap_classes)
    
    if (default_config$add_last_layer) {
      
      last_layer_info <- info %>% define_last_layer(units = output_width ^ 3,
                                                    only_convolutionals = only_convolutionals, 
                                                    force_categorical = TRUE,
                                                    hidden_layers = default_config$last_hidden_layers)
      
    } else {
      
      last_layer_info <- NULL
      
    }
    
    to_add$last_layer_info <- last_layer_info
    to_add$vol_layers <- info %>% create_vol_layers(default_config$vol_layers_pattern)
    
    
    config <- do.call(define_config, args = c(scheme_list, to_add))
    
  }
  
  return(config)
  
}
