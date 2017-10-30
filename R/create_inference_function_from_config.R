#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param config    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso 
#'  \code{\link[keras]{to_categorical}}
#' @export 
#' @importFrom keras to_categorical
#' @import progress
create_inference_function_from_config <- function(config) {
  
  
  if ("DLconfig" %in% class(config)) {
    
    radius <- 0.5 * (config$width + 1)
    
    f_inference <- function(model, V, speed = c("slower", "medium", "faster"), ...) {
      
      num_inputs <- length(V)
      
      if (!("DLmodel" %in% class(model))) {
        
        stop("Not a DLmodel object")
        
      } else {
        
        .model <- model$model
        
      }
      
      stride <- switch(speed, 
                       "slower" = 1,
                       "medium" = (config$output_width + 1) / 2,
                       "faster" = config$output_width)
      
      meanX <- list()
      stdX <- list()
      maxX <- list()
      
      for (input in seq(num_inputs)) {
        
        if (config$scale %in% c("mean", "z", "meanmax")) meanX[[input]] <- mean(as.vector(V[[input]]))
        if (config$scale %in% "z") stdX[[input]] <- sd(as.vector(V[[input]]))
        if (config$scale %in% c("max", "meanmax")) maxX[[input]] <- max(as.vector(V[[input]]))
        
      }
      
      V0 <- V[[1]] * 0
      V0[seq(from = radius, to = dim(V0)[1] - (radius + 1), by = stride),
         seq(from = radius, to = dim(V0)[2] - (radius + 1), by = stride),
         seq(from = radius, to = dim(V0)[3] - (radius + 1), by = stride)] <- 1
      all_idx <- which(V0 > 0)
      
      if ((config$categorize_output) && (config$category_method == "by_class")) {
        
        res <- array(0, dim = c(dim(V[[1]]), config$last_layer$params$num_classes))
        
      } else {
        
        res <- V[[1]] * 0
        
      }
      
      counts <- V[[1]] * 0
      last_distance <- V[[1]] * 0 + 3 * width ^ 2
      
      # Must define num_windows
      num_windows <- round(unclass(config$memory_limit / 
                                     object.size(vector(mode = "double",
                                                        length = config$width ^ 3 + 
                                                          config$num_features + 
                                                          config$output_width ^ 3))))
      
      num_windows <- round(num_windows / (num_inputs + 2))
      
      sampling_indices <- all_idx
      num_batches <- ceiling(length(sampling_indices) / num_windows)
      if (num_batches > 1)
        batch_idx <- as.numeric(cut(seq_along(sampling_indices), num_batches))
      else
        batch_idx <- rep(1, times = length(sampling_indices))
      
      if (require(progress)) {
        
        progress <- TRUE
        pb_infer <- progress_bar$new(format = " Batch :batch/:total [:bar] ETA: :eta . Elapsed: :elapsed",
                                     total = num_batches,
                                     clear = FALSE,
                                     width = 60)
        
        pb_infer$update(ratio = 0, tokens = list(batch = 0))
        
      } else {
        
        progress <- FALSE
        
      }
      
      for (batch in seq(num_batches)) {
        
        if (!progress)
          message("Batch ", batch, " out of ", num_batches)
        
        idx <- sampling_indices[batch_idx == batch]
        coords <- idx %>% arrayInd(.dim = dim(V[[1]]))
        
        x <- coords[, 1] - 1
        y <- coords[, 2] - 1
        z <- coords[, 3] - 1
        
        X_vol <- list()
        
        for (input in seq(num_inputs)) {
          
          X <- get_windows_at(V[[input]], config$width, x, y, z)
          X_coords <- X[, 1:3]
          X_coords[, 1] <- X_coords[, 1] / dim(V[[input]])[1]
          X_coords[, 2] <- X_coords[, 2] / dim(V[[input]])[2]
          X_coords[, 3] <- X_coords[, 3] / dim(V[[input]])[3]
          
          X_vol[[input]] <- X[, -c(1:3)]
          
          if (config$is_autoencoder) {
            
            if (config$categorize_input) {
              
              if (!is.null(config$remap_classes)) {
                
                X_vol[[input]][!(X_vol[[input]] %in% config$y_label)] <- 0
                
                s <- config$remap_classes$source
                t <- config$remap_classes$target
                
                X_vol_ <- X_vol[[input]]
                
                for (i in seq_along(s)) {
                  
                  X_vol_[X_vol[[input]] == s[i]] <- t[i]
                  
                }
                
                X_vol[[input]] <- X_vol_
                
              }
              
              # X_vol[!(X_vol %in% y_label)] <- 0
              
              X_vol2 <- keras::to_categorical(X_vol[[input]], num_classes = config$num_classes)
              X_vol[[input]] <- t(matrix(t(X_vol2), nrow = config$width ^ 3 * config$num_classes))
              
            } else {
              
              if (config$scale == "none") {
                
                if (config$binarise) {
                  
                  X_vol[[input]][!(X_vol[[input]] %in% config$y_label)] <- -1
                  X_vol[[input]][X_vol[[input]] %in% config$y_label] <- 1
                  
                } else {
                  
                  X_vol[[input]][!(X_vol %in% config$y_label)] <- 0
                  
                  if (!is.null(config$remap_classes)) {
                    
                    s <- config$remap_classes$source
                    t <- config$remap_classes$target
                    
                    X_vol_ <- X_vol[[input]]
                    
                    for (i in seq_along(s)) {
                      
                      X_vol_[X_vol[[input]] == s[i]] <- t[i]
                      
                    }
                    
                    X_vol[[input]] <- X_vol_
                    
                  }
                  
                }
                
              } else {
                
                switch(config$scale,
                       "none" = X_vol[[input]] <- X_vol[[input]],
                       "mean" = X_vol[[input]] <- X_vol[[input]] - meanX[[input]],
                       "z"    = X_vol[[input]] <- (X_vol[[input]] - meanX[[input]]) / stdX[[input]],
                       "max"  = X_vol[[input]] <- X_vol[[input]] / maxX[[input]],
                       "meanmax" = X_vol[[input]] <- 
                         (X_vol[[input]] - meanX[[input]]) / (maxX[[input]] - meanX[[input]]),
                       "110" = X_vol[[input]] <- X_vol[[input]] - 110)
                
              }
              
            }
            
          } else {
            
            switch(config$scale,
                   "none" = X_vol[[input]] <- X_vol[[input]],
                   "mean" = X_vol[[input]] <- X_vol[[input]] - meanX[[input]],
                   "z"    = X_vol[[input]] <- (X_vol[[input]] - meanX[[input]]) / stdX[[input]],
                   "max"  = X_vol[[input]] <- X_vol[[input]] / maxX[[input]],
                   "meanmax" = X_vol[[input]] <- (X_vol[[input]] - meanX[[input]]) / (maxX[[input]] - meanX[[input]]),
                   "110" = X_vol[[input]] <- X_vol[[input]] - 110)
            
          }
          
          
        }
        
        inputs <- c(list(X_coords), X_vol)
        
        output <- .model %>% predict_on_batch(x = inputs)
        
        if (config$categorize_output) {
          
          # Should be a categorical layer
          if (config$last_layer$type == "categorical") {
            
            num_classes <- config$last_layer$params$num_classes
            units <- config$last_layer$params$units
            
            output <- Reduce(cbind, output)

            dims <- dim(output)
            
            if (config$category_method == "simple") {
              
              new_output <- array(t(output), dim = c(num_classes, units * dims[1]))
              classes <- apply(new_output, 2, which.max) - 1
              output <- t(array(classes, dim = c(units, dims[1])))
              
              if (config$output_width > 1) {
                
                results_to_volume_label_with_distance(V = output, 
                                                      width = config$output_width,
                                                      res = res, 
                                                      last_distance = last_distance,  
                                                      x = x, 
                                                      y = y, 
                                                      z = z)
                
              } else {
                
                res[idx] <- output
                
              }
              
            } else {
              
              # new_output <- array(output, dim = c(dim[1], num_classes, units))
              
              for (k in seq(num_classes)) {
                
                new_output <- output[, seq(from = k, to = num_classes * units, by = num_classes)]
                
                res_ <- res[, , , k]
                
                if (config$output_width > 1) {
                  
                  results_to_volume(V = new_output, 
                                    width = config$output_width,
                                    res = res_,  
                                    counts = counts,
                                    x = x, 
                                    y = y, 
                                    z = z)
                  
                } else {
                  
                  res_[idx] <- new_output
                  
                }
                
                res[, , , k] <- res_
                
              } 
              
            }
            
          }
          
        } else {
          
          # if its a multivalued layer, treat as a categorical layer
          if (config$last_layer$type == "multivalued") {
            
            if (config$output_width > 1) {
              
              results_to_volume_label_with_distance(V = round(output), 
                                                    width = config$output_width,
                                                    res = res,  
                                                    last_distance = last_distance,  
                                                    x = x, 
                                                    y = y, 
                                                    z = z)
              
            } else {
              
              res[idx] <- round(output)
              
            }
            
          } else {
            
            if (config$output_width > 1) {
              
              results_to_volume(V = output, 
                                width = config$output_width,
                                res = res,  
                                counts = counts,
                                x = x, 
                                y = y, 
                                z = z)
              
            } else {
              
              res[idx] <- output
              
            }
            
          }
          
          
        }
        
        if (progress) {
          
          pb_infer$tick(tokens = list(batch = batch))
          
        }
        
      }
      
      which_to_divide <- which(counts > 0)
      
      if (length(which_to_divide) > 0) {
        
        if ((config$categorize_output) & (config$category_method == "by_class")) {
          
          counts <- counts / num_classes
          
          for (k in seq(num_classes)) {
            
            res_ <- res[, , , k]
            res_[which_to_divide] <- res_[which_to_divide] / counts[which_to_divide]
            res[, , , k] <- res_
            
          }
          
          # assign(x = "res_", value = res, envir = .GlobalEnv)
          
          
        } else {
          
          res[which_to_divide] <- res[which_to_divide] / counts[which_to_divide]
          
        }
        
      }
      
      
      if ((!config$categorize_output) | (config$categorize_output & config$category_method == "by_class"))
        if (config$regularize) {
          
          kernel <- c(0.0325130027975867, 0.0368420588246428, 0.0325130027975867, 
                      0.0368420588246428, 0.041747521964941, 0.0368420588246428, 
                      0.0325130027975867, 0.0368420588246428, 0.0325130027975867, 
                      0.0368420588246428, 0.041747521964941, 0.0368420588246428, 
                      0.041747521964941, 0.0473061399339463, 0.041747521964941, 
                      0.0368420588246428, 0.041747521964941, 0.0368420588246428, 
                      0.0325130027975867, 0.0368420588246428, 0.0325130027975867, 
                      0.0368420588246428, 0.041747521964941, 0.0368420588246428, 
                      0.0325130027975867, 0.0368420588246428, 0.0325130027975867 )
          kernel <- array(kernel, dim = c(3, 3, 3))
          res <- regularize(res, kernel)

        }
      
      if ((config$categorize_output) & (config$category_method == "by_class")) {
        
        res <- apply(res, c(1:3), which.max) - 1
        
      }
      
      if (config$categorize_output) {
        
        res_ <- 0 * res
        if (!is.null(config$remap_classes)) {
          
          s <- config$remap_classes$source
          ta <- config$remap_classes$target
          
          for (cl in seq_along(s)) {
            
            res_[res == ta[cl]] <- s[cl]
            
          }
          
          res <- res_
          
        }
        
      }
      
      return(res)
      
    }
    
    
    return(f_inference)
    
  } else {
    
    return(NULL)
    
  }
  
}
