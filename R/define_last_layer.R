#' @title Define the Last Layer of a Model according to Problem Type
#'
#' @description This function uses the information about a problem to define
#' a possible last layer.
#'
#' @param info                 (environment) Problem information as returned by \code{\link{get_problem_info}}.
#' @param units                (integer) Number of output units
#' @param only_convolutionals  (logical) If \code{TRUE}, just returns convolutional layers, otherwise dense layers, Default: FALSE
#' @param force_categorical    (logical) When only 2 classes in a classification problem, one can use a \code{softsign} regression or a categorical layer, Default: TRUE 
#' @param multioutput          (logical) Split the output units, each one with a loss function, Default: TRUE
#' @param ...                  arguments passed to other functions.
#'
#' @return A list with:
#' - \code{last_layer}, The definition of the last layer,
#' - \code{output_activation}, The output activation function,
#' - \code{loss}, The loss function(s) to use for each output,
#' - \code{remap_classes}, A mapping from the class numbers in the original images and 1:num_classes,
#' - \code{binarise}, A logical indicating if there are only 2 classes and not using a categorical output,
#' - \code{num_classes}, The number of classes, in a classification problem,
#' - \code{y_label}, The set of labels in the original output image in a classification problem,
#' - \code{multioutput}, A logical to indicate if the output is split.
#'
#' @export 
#' 
define_last_layer <- function(info, 
                              units, 
                              only_convolutionals = FALSE,
                              force_categorical = TRUE, 
                              multioutput = TRUE, ...) {
  
  # Basic input class checking
  stopifnot(inherits(info, "DLproblem"))
  
  # Initialize variables
  binarise <- FALSE
  num_classes <- 0
  
  extra_args <- list(...)
  
  last_layer_info <- switch(info$type,
                            
                            "image_labelling" = {
                              
                              # If it's a classification problem
                              # Get the number of classes and their labels
                              num_classes <- length(info$values)
                              
                              loss_function <- keras::loss_categorical_crossentropy
                              if (num_classes == 1)
                                loss_function <- keras::loss_binary_crossentropy
                              
                              if ("loss_function" %in% names(extra_args)) {
                                
                                loss_function <- extra_args$loss_function
                                
                              }
                              
                              if (is.null(info$subsetted)) {
                                
                                y_label <- info$values
                                remap_classes <- info$remap_classes
                                
                              } else {
                                
                                if (!info$subsetted) {
                                  
                                  y_label <- info$values
                                  remap_classes <- list(source = info$values, 
                                                        target = seq_along(info$values))
                                  
                                } else {
                                  
                                  y_label <- info$original_values
                                  remap_classes <- info$remap_classes
                                  
                                }
                                
                              }             
                              
                              if (only_convolutionals) {
                                
                                output_activation <- "softmax"
                                last_layer <- conv3d(filters = num_classes + 1,
                                                     kernel_size = c(3, 3, 3),
                                                     padding = "same",
                                                     activation = output_activation)
                                multioutput <- FALSE
                                loss <- loss_function
                                
                                
                              } else {
                                
                                # If more than 2 classes, only option is a categorical layer as output.
                                if (num_classes > 1) {
                                  
                                  # If multioutput (split the output in different units)
                                  if (multioutput) {
                                    
                                    # Categorical layer with categorical_crossentropy as loss
                                    last_layer <- categorical(num_classes = num_classes + 1, 
                                                              units = units, ...)
                                    
                                    loss <- list()
                                    for (i in seq(units)) {
                                      
                                      loss[[i]] <- loss_function
                                      
                                    }
                                    
                                  } else {
                                    
                                    # Categorical layer with mean_squared_error as loss
                                    last_layer <- categorical(num_classes = num_classes + 1, 
                                                              units = units, 
                                                              concatenate = TRUE, ...)
                                    
                                    loss <- keras::loss_mean_squared_error
                                    
                                  }
                                  
                                  output_activation <- "sigmoid"
                                  
                                } else {
                                  
                                  # Only two classes. We can force a categorical layer or a 
                                  # simple dense layer with softsign activation.
                                  if (force_categorical) {
                                    
                                    # As before, but with binary_crossentropy
                                    if (multioutput) {
                                      
                                      last_layer <- categorical(num_classes = num_classes + 1, 
                                                                units = units, ...)
                                      
                                      loss <- list()
                                      for (i in seq(units)) {
                                        
                                        loss[[i]] <- loss_function
                                        
                                      }
                                      
                                    } else {
                                      
                                      last_layer <- categorical(num_classes = num_classes + 1, 
                                                                units = units, 
                                                                concatenate = TRUE, ...)
                                      
                                      loss <- keras::loss_mean_squared_error
                                      
                                    }
                                    
                                    output_activation <- "sigmoid"
                                    
                                  } else {
                                    
                                    # 2 classes mapped to -1, +1. Use softsign.
                                    last_layer <- dense(units = units)
                                    output_activation <- "softsign"
                                    binarise <- TRUE
                                    loss <- keras::loss_mean_squared_error
                                    
                                  }
                                  
                                }
                                
                              }
                              
                              
                              list(last_layer = last_layer, 
                                   output_activation = output_activation,
                                   loss = loss,
                                   remap_classes = remap_classes,
                                   binarise = binarise,
                                   num_classes = num_classes,
                                   num_volumes = info$num_volumes,
                                   y_label = y_label, 
                                   multioutput = multioutput,
                                   categorize_output = TRUE)
                              
                            },
                            
                            "image_regression" = {
                              
                              # If the problem is not classification, it is regression.
                              # Thus, output activation should be linear.
                              # No classes, like in communism.
                              
                              output_activation <- "relu"
                              multioutput <- FALSE
                              
                              remap_classes <- NULL
                              y_label <- NULL
                              binarise <- FALSE
                              
                              # Regression layer.
                              if (only_convolutionals) {
                                
                                last_layer <- conv3d(filters = 1,
                                                     kernel_size = c(3, 3, 3),
                                                     padding = "same",
                                                     activation = output_activation)
                                
                              } else {
                                
                                last_layer <- regression(units = units, 
                                                         output_activation = output_activation, 
                                                         ...)
                                
                                
                              }
                              
                              loss <- keras::loss_mean_squared_error
                              
                              list(last_layer = last_layer, 
                                   output_activation = output_activation,
                                   loss = loss,
                                   remap_classes = remap_classes,
                                   binarise = binarise,
                                   num_classes = num_classes,
                                   num_volumes = info$num_volumes,
                                   y_label = y_label, 
                                   multioutput = multioutput,
                                   categorize_output = FALSE)
                              
                            },
                            
                            "subject_classification" = {
                              
                              
                            },
                            
                            "subject_regression" = {
                              
                              
                            })
  
  return(last_layer_info)
  
}
