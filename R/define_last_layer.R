#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param info                 (name) PARAM_DESCRIPTION
#' @param units                (name) PARAM_DESCRIPTION
#' @param force_categorical    (name) PARAM_DESCRIPTION
#' @param multioutput          (logical) PARAM_DESCRIPTION
#' @param ...                  (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
define_last_layer <- function(info, units, force_categorical = TRUE, multioutput = TRUE, ...) {
  
  binarise <- FALSE
  num_classes <- 0
  
  if (info$type == "image_labelling") {
    
    num_classes <- length(info$values)
    y_label <- info$values
    
    remap_classes <- list(source = info$values, 
                          target = seq_along(info$values))
    
    if (num_classes > 1) {
      
      if (multioutput) {
        
        last_layer <- categorical(num_classes = num_classes + 1, 
                                  units = units, ...)
        
        loss <- list()
        for (i in seq(units)) {
          
          loss[[i]] <- keras::loss_categorical_crossentropy
          
        }
        
      } else {
        
        last_layer <- categorical(num_classes = num_classes + 1, 
                                  units = units, 
                                  concatenate = TRUE, ...)
        
        loss <- keras::loss_mean_squared_error
        
      }
      
      output_activation <- "sigmoid"
      
    } else {
      
      if (force_categorical) {
        
        if (multioutput) {
          
          last_layer <- categorical(num_classes = num_classes + 1, 
                                    units = units, ...)
          
          loss <- list()
          for (i in seq(units)) {
            
            loss[[i]] <- keras::loss_categorical_crossentropy
            
          }
          
        } else {
          
          last_layer <- categorical(num_classes = num_classes + 1, 
                                    units = units, 
                                    concatenate = TRUE, ...)
          
          loss <- keras::loss_mean_squared_error
          
        }
        
        output_activation <- "sigmoid"
        
      } else {
        
        last_layer <- dense(units = units)
        output_activation <- "softsign"
        binarise <- TRUE
        loss <- keras::loss_mean_squared_error
        
      }
      
    }
    
  } else {
    
    output_activation <- "linear"
    multioutput <- FALSE
    
    remap_classes <- NULL
    y_label <- NULL
    binarise <- FALSE
    
    if (info$range[2] == 1) {
      
      if (info$range[1] == -1) {
        
        output_activation <- "softsign"
        
      } else {
        
        if (info$range[1] == 0) output_activation <- "sigmoid"
        
      }
      
    }
    
    last_layer <- regression(units = units, 
                             output_activation = output_activation, 
                             ...)
    
    loss <- keras::loss_mean_squared_error
    
  }
  
  return(list(last_layer = last_layer, 
              output_activation = output_activation,
              loss = loss,
              remap_classes = remap_classes,
              binarise = binarise,
              num_classes = num_classes,
              y_label = y_label, 
              multioutput = multioutput))
  
}
