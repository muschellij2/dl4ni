#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param flow              (name) PARAM_DESCRIPTION
#' @param epochs            (numeric) PARAM_DESCRIPTION, Default: 10
#' @param max_sub_epochs    (numeric) PARAM_DESCRIPTION, Default: 5
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
train_output <- function(flow, 
                         output, 
                         input_filenames,
                         output_filenames,
                         given_input = NULL,
                         train_split = 0.75,
                         epochs = 10, 
                         max_sub_epochs = 5,
                         verbose = TRUE) {
  
  # Basic check
  stopifnot(inherits(flow, "DLflow"))
  stopifnot(output %in% flow$outputs)
  
  # Check that files exist
  stopifnot(all(file.exists(unlist(input_filenames))), all(file.exists(unlist(output_filenames))))
  
  if (verbose)
    cat("Checking previous steps are trained...\n")
  
  # Check that previous steps in the pipeline are trained
  needed_outputs <- flow$inmediate_inputs[[output]]
  # previous_outputs <- needed_outputs
  
  given_input <- c(input_filenames, given_input)
  
  given_names <- names(given_input)
  
  pipeline <- flow$pipeline[[output]]
  pipeline <- flow$outputs[pipeline]
  
  to_compute <- flow %>% which_to_compute(output = output, given_inputs = given_names)
  
  processes <- intersect(pipeline, to_compute)
  inputs_to_use <- intersect(to_compute, flow$inputs)
  
  all_trained <- all(unlist(flow$trained[processes]))
  if (!all(all_trained)) {
    
    stop("Not all previous models are trained.")
    
  }
  
  if (length(inputs_to_use) > 0) {
    
    stop("Not all needed inputs have been provided.")
    
  }
  
  if (verbose)
    cat("   Everything is Ok...\n")
  
  # Mark as trained in the output
  on.exit({
    
    flow$trained[[output]] <- TRUE
    
  })
  
  model <- flow$processes[[output]]
  
  if (inherits(model, "DLmodel") & !flow$trained[[output]]) {
    
    # Temporary results
    tmp_folder <- tempdir()
    
    num_subjects <- length(output_filenames)
    
    # Obtain the results of the needed previous steps
    desired_outputs <- needed_outputs 
    
    if (verbose)
      cat("Obtaining required inputs...\n")
    
    results <- list()
    
    # For each subject
    for (s in seq(num_subjects)) {
      
      if (verbose)
        cat("Subject number", s, "out of", num_subjects, "...\n")
      
      # Input files for this subject
      input_file_list <- lapply(given_input, function(x) x[s])
      
      # Execute the flow to get the previous required results (they are the inputs to 
      # the block we are about to train)
      previous_results <- flow %>% execute_flow(inputs = input_file_list, 
                                                desired_outputs = desired_outputs, 
                                                initialize_outputs = FALSE)
      
      # Reset computed outputs for next interation
      flow %>% reset_outputs()
      
      # Save the results in a temp folder and store its location in the results list
      filenames <- paste0(desired_outputs, "_", s)
      
      for (f in seq(filenames)) {
        
        given_output <- desired_outputs[f]
        
        if (inherits(previous_results[[given_output]], "nifti")) {
          
          filenames[f] <- paste0(filenames[f], ".nii.gz")
          
          neurobase::writenii(nim = neurobase::niftiarr(arr = previous_results[[given_output]], 
                                                        img = neurobase::readnii(input_file_list[[1]])),
                              filename = file.path(tmp_folder, filenames[f]))
          
        } else {
          
          filenames[f] <- paste0(filenames[f], ".rds")
          saveRDS(previous_results[[given_output]], file = file.path(tmp_folder, filenames[f]))
          
        }
        
        results[[given_output]] <- c(results[[given_output]], file.path(tmp_folder, filenames[f]))
        
      }
      
    }
    
    if (verbose)
      cat("Training configuration...\n")
    
    # Model configuration
    config <- model$hyperparameters

    # Training configuration
    train_indices <- sample(seq(num_subjects), size = round(train_split * num_subjects))
    test_indices <- setdiff(seq(num_subjects), train_indices)
    
    train_config <- config %>% create_generator_from_config(x_files = lapply(results, function(x) x[train_indices]),
                                                            y_files = output_filenames[train_indices],
                                                            mode = "sampling",
                                                            max_sub_epochs = max_sub_epochs)
    
    test_config <- config %>% create_generator_from_config(x_files = lapply(results, function(x) x[test_indices]),
                                                           y_files = output_filenames[test_indices],
                                                           mode = "sampling",
                                                           max_sub_epochs = max_sub_epochs)
    
    keep_best <- TRUE
    saving_path <- file.path(system.file(package = "dl4ni"), "models")
    saving_prefix <- paste0("flow_", flow$name, "_", output, "_", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"))
    
    # Actual training
    if (verbose)
      cat("Actual training...\n")
    model %>% fit_with_generator(train_config = train_config, 
                                 validation_config = test_config,
                                 epochs = epochs,
                                 keep_best = keep_best,
                                 path = saving_path,
                                 prefix = saving_prefix)
    
    if (verbose)
      cat("Done.\n")
    
  }
  
  return(invisible(flow))
  
}
