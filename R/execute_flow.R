#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param flow               (name) PARAM_DESCRIPTION
#' @param inputs             (call) PARAM_DESCRIPTION, Default: list()
#' @param desired_outputs    (NULL) PARAM_DESCRIPTION, Default: NULL
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
execute_flow <- function(flow, inputs = list(), desired_outputs = NULL) {
  
  stopifnot(inherits(flow, "DLflow"))
  
  # Check that inputs is a named list of files and that all of them exist
  all_exist <- all(sapply(inputs, file.exists))
  
  if (!all_exist)
    stop("Not all input files exist.")
  
  input_names <- names(inputs)
  input_names <- input_names[input_names %in% flow$inputs]
  
  # Initialize computed_outputs
  flow$computed_outputs <- list()
  
  # Check that the desired outputs can be computed
  all_computable <- all(desired_outputs %in% flow$outputs)
  if (!all_computable)
    warning("Some of the outputs cannot be computed.")
  
  desired_outputs <- desired_outputs[desired_outputs %in% flow$outputs]
  
  results <- list()
  
  if (length(desired_outputs) > 0) {
    
    # Read inputs
    for (name in input_names) {
      
      flow$computed_outputs[[name]] <- neurobase::readnii(inputs[[name]])
      
    }
    
    # For each output
    for (output in desired_outputs) {
      
      # Are all the required inputs?
      inputs_idx <- unique(flow$required_inputs[[output]])
      all_required <- all(V(flow$graph)$name[inputs_idx] %in% input_names)
      
      if (!all_required) {
        
        message <- paste0("Not all required inputs for ", output)
        warning(message)
        
        next
        
      }
      
      # Define which parts of the flow must be processed
      pipeline <- flow$pipeline[[output]]
      
      # Execute in order
      for (process_idx in pipeline) {
        
        intermediate_output <- flow$outputs[process_idx]
        
        # if this process is already computed, go to the next one
        if (!is.null(flow$computed_outputs[[intermediate_output]])) next
        
        cat("Computing", intermediate_output, "...\n")
        
        process <- flow$processes[[intermediate_output]]
        my_inputs <- flow$inmediate_inputs[[intermediate_output]]
        
        switch(V(flow$graph)$type[process_idx],
               
               "function" = {
                 
                 params <- flow$computed_outputs[my_inputs]
                 names(params) <- names(formals(process))
                 flow$computed_outputs[[intermediate_output]] <- do.call(what = process, args = params)
                 
               },
               
               "DLmodel" = {
                 
                 # Inference function for the given model
                 infer <- process$hyperparameters %>% create_inference_function_from_config()
                 
                 # Infer on input volumes
                 input_imgs <- flow$computed_outputs[my_inputs]
                 flow$computed_outputs[[intermediate_output]] <- process %>% infer(V = input_imgs, spped = "medium")
                 
               })
        
      }
      
      
    }
    
    results <- flow$computed_outputs[desired_outputs]
    
  }
  
  
  return(results)
  
}
