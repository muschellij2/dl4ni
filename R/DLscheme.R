DLscheme <- R6::R6Class(
  
  classname = "DLscheme",
  
  
  public = list(
    
    initialize = function() { },
    
    add = function(...) {
      
      self %>% add_attribute(...)
      
    },
    
    instantiate = function(problem_info = NULL, 
                           inputs = NULL, 
                           outputs = NULL, 
                           labels_subset = NULL) {
      
      if (is.null(problem_info)) {
        
        if (is.null(inputs) | is.null(outputs)) {
          
          stop("problem_info, inputs and outputs cannot be all NULL.")
          
        }
        
        problem_info <- analyze_input(input = inputs) %>% analyze_output(output = outputs)
        
      }
      
      model <- self %>% instantiate_model(problem_info, labels_subset = labels_subset)
      
      if (!is.null(problem_info$train)) {
        
        model$use_data(use = "train",
                       x_files = problem_info$train$x,
                       y_files = problem_info$train$y,
                       target_windows_per_file = 1024)
        
      }
      
      if (!is.null(problem_info$test)) {
        
        model$use_data(use = "test",
                       x_files = problem_info$test$x,
                       y_files = problem_info$test$y,
                       target_windows_per_file = 1024)
        
      }
      
      return(model)
      
    },
    
    to_list = function() {
      
      result <- as.list(self)
      result$clone <- NULL
      result$initialize <- NULL
      result$add <- NULL
      result$to_list <- NULL
      result$instantiate <- NULL
      result$.__enclos_env__ <- NULL
      
      return(result)
      
    },
    
    from_list = function(config) {
      
      stopifot(is.list(config))
      
      # Remove particular elements that are going to be appended in the instantiation phase
      config$vol_layers <- NULL
      config$last_layer_info <- NULL
      config$num_inputs <- NULL
      config$num_volumes <- NULL
      
      do.call(self$add, args = config)
      
    },
    
    from_model = function(model) {
      
      stopfinot(inherits(model, "DLmodel"))
      
      config <- model$get_config()
      
      self$from_list(config)
      
    }
    
    
  ),
  
  lock_objects = FALSE
  
)
