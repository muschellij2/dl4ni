#' DLscheme Class
#'
#' @docType class
#' @importFrom R6 R6Class
#'
#' @export
#' @keywords data
#'
#' @return Object of \code{\link{R6Class}} and \code{DLscheme}.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @examples
#' DLscheme$new()
#'
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method follow the corresponding link. }
#'   \item{\code{initialize()}}{METHOD DESCRIPTION. Documented in \link{DLscheme.initialize}.}
#'   \item{\code{add(...)}}{METHOD DESCRIPTION. Documented in \link{DLscheme.add}.}
#'   \item{\code{instantiate(problem_info = NULL, inputs = NULL, outputs = NULL, labels_subset = NULL)}}{METHOD DESCRIPTION. Documented in \link{DLscheme.instantiate}.}
#'   \item{\code{to_list()}}{METHOD DESCRIPTION. Documented in \link{DLscheme.to_list}.}
#'   \item{\code{from_list(config)}}{METHOD DESCRIPTION. Documented in \link{DLscheme.from_list}.}
#'   \item{\code{from_model(model)}}{METHOD DESCRIPTION. Documented in \link{DLscheme.from_model}.}
#'   \item{\code{clone(deep = FALSE)}}{METHOD DESCRIPTION. Documented in \link{DLscheme.clone}.}
#'  }
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
        
        if (is.null(inputs)) {
          
          stop("problem_info, and inputs cannot be both NULL.")
          
        } else {
          
          if (!is.null(self$is_autoencoder) && self$is_autoencoder & is.null(outputs)) {
            
            outputs <- inputs[[1]]
            
          }
          
          if (is.null(outputs)) {
            
            stop("outputs can only be NULL when building an autoencoder.")
            
          }
          
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
