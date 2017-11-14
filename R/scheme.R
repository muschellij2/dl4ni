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
  
  lapply(var_names, function(vn) {
    
    assign(x = vn, value = args[[vn]], envir = scheme)
    return(invisible())
    
  })

  return(invisible(scheme))
  
}

instantiate_model_config <- function(scheme, info = NULL, inputs = NULL, outputs = NULL, labels_subset = NULL) {
  
  # Basic input checks
  stopifnot(inherits(scheme, "DLscheme"))
  stopifnot(!is.null(info) | (!is.null(inputs) & !is.null(outputs)))

  default_config <- get_dl4ni_config()
  scheme_list <- as.list(scheme)
  
  # Overwrite defaults
  for (nm in names(scheme_list)) {
    
    if (!is.null(scheme_list[[nm]]))
      default_config[[nm]] <- scheme_list[[nm]]
    
  }
  
  output_width <- default_config$output_width
  only_convolutionals <- default_config$only_convolutionals
  
  if (is.null(info)) {
    
    # Create info from data provided
    info <- analyze_input(input = inputs) %>% analyze_output(output = outputs)
    
  }
  
  if (!is.null(info)) {
    
    if (!is.null(labels_subset))
      info <- do.call(subset_problem, args = labels_subset)
    
    to_add <- list(num_inputs = info$num_inputs,
                   num_volumes = info$num_volumes,
                   remap_classes = info$remap_classes)
    
   
    last_layer_info <- info %>% define_last_layer(units = output_width ^ 3,
                                                  only_convolutionals = only_convolutionals, 
                                                  force_categorical = TRUE,
                                                  hidden_layers = scheme$last_hidden_layers)
    
    to_add$last_layer_info <- last_layer_info
    to_add$vol_layers <- info %>% create_vol_layers(default_config$vol_layers_pattern)
    
     
    config <- do.call(define_config, args = c(scheme_list, to_add))
    
  }
  
  return(config)
  
}
