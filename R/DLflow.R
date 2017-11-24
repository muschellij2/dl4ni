DLflow <- R6::R6Class(
  
  classname = "DLflow",
  
  
  public = list(
    
    initialize = function(name = "", inputs = list()) {
      
      flow_env <- create_flow(name = name, inputs = inputs)
      
      self$.__enclos_env__$private <- flow_env
      
    },
    
    name = function() {
      
      private$name
      
    },
    
    get_inputs = function() {
      
      my_flow <- self$.__enclos_env__$private
      my_flow$inputs
      
    },
    
    get_outputs = function() {
      
      my_flow <- self$.__enclos_env__$private
      my_flow$outputs
      
    },
    
    get_model = function(output) {
      
      my_flow <- self$.__enclos_env__$private
      
      return(my_flow$processes[[output]])
      
    },
    
    replace = function(output, with) {
      
      if (!(output %in% self$get_outputs())) {
        
        message <- paste0("No current definition for output = ", output)
        stop(message)
        
      }
      
      my_flow <- self$.__enclos_env__$private
      my_flow$processes[[output]] <- with
      
      if (inherits(with, "DLscheme")) {
        
        my_flow$schemes[[output]] <- with
        
      }
      
      if (inherits(with, "DLmodel")) {
        
        scheme <- DLscheme$new()
        scheme$from_model(with)
        
        my_flow$schemes[[output]] <- scheme
        
      }
      
    },
    
    add = function(what = NULL, inputs = NULL, output = NULL, subset = NULL) {
      
      my_flow <- self$.__enclos_env__$private
      
      # Add an input
      if (is.null(what) & is.null(output)) {
        
        if (is.null(inputs)) {
          
          stop("At least inputs must be specified.")
          
        }
        
        my_flow %>% add_inputs(inputs = inputs)
        
        return(invisible(self))
        
      }
      
      # To add an scheme, we need inputs and output
      if (inherits(what, "DLscheme")) {
        
        if (is.null(inputs) | is.null(output)) {
          
          stop("To add a scheme, please provide inputs and output.")
          
        }
        
        my_flow %>% add_trainable_model(scheme = what$clone(), inputs = inputs, output = output, subset = subset)
        
        return(invisible(self))
        
      }
      
      # Add a DLmodel or a function
      if (inherits(what, "DLmodel") | inherits(what, "function")) {
        
        if (is.null(output)) {
          
          stop("An output must be provided to add a DLmodel or a function.")
          
        }
        
        if (is.null(inputs)) {
          
          if (inherits(what, "DLmodel")) {
            
            stop("Inputs must be provided to add a DLmodel.")
            
          } else {
            
            my_flow %>% add_process(proc = what, output = output)
            
          }
          
        } else {
          
          my_flow %>% add_process(proc = what, inputs = inputs, output = output)
          
        }
        

      }
      
    },
    
    execute = function(inputs = list(),
                       desired_outputs = NULL, 
                       initialize_outputs = TRUE,
                       mode = c("debug", "faster", "medium", "slower")) {
      
      my_flow <- self$.__enclos_env__$private
      my_flow %>% execute_flow(inputs = inputs, 
                               desired_outputs = desired_outputs, 
                               initialize_outputs = initialize_outputs,
                               mode = mode[1])
      
    },
    
    run = function(...) {
      
      self$execute(...)
      
    },
    
    train = function(output, 
                     input_filenames,
                     output_filenames,
                     train_split = 0.75,
                     epochs = 10, 
                     target_windows_per_file = 1024,
                     mode = c("debug", "faster", "medium", "slower")) {
      
      my_flow <- self$.__enclos_env__$private
      my_flow %>% train_output(output = output, 
                               input_filenames = input_filenames,
                               output_filenames = output_filenames,
                               train_split = train_split,
                               epochs = epochs, 
                               target_windows_per_file = target_windows_per_file,
                               mode = mode[1])
      
    },
    
    graph = function() {
      
      my_flow <- self$.__enclos_env__$private
      return(my_flow$graph)
      
    },
    
    plot = function(interactive = FALSE) {
      
      my_flow <- self$.__enclos_env__$private
      my_flow %>% plot_flow(interactive = interactive)
      
    },
    
    reset = function(outputs = "all") {
      
      my_flow <- self$.__enclos_env__$private
      my_flow %>% reset_flow(outputs = outputs)
      
      return(invisible(self))
      
    },
    
    save = function(path = tempdir(), file_prefix = self$name()) {
      
      my_flow <- self$.__enclos_env__$private
      my_flow %>% save_flow(path = path, file_prefix = file_prefix)
      
    },
    
    load = function(filename) {
      
      self$.__enclos_env__$private <- load_flow(filename)
      
    },
    
    subset = function(outputs) {
      
      my_flow <- self$.__enclos_env__$private
      new_flow_env <- my_flow %>% subset_flow(outputs = outputs)
      
      new_flow <- DLflow$new(name = self$name())
      new_flow$.__enclos_env__$private <- new_flow_env
      
      return(new_flow)
      
    }
    
  ),
  
  lock_objects = FALSE

)
