#' @title Generic Inference Function for a Model Configuration
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
create_inference_function_from_config <- function(object) {
  
  stopifnot(inherits(object, "DLconfig") | inherits(object, "DLmodel"))
  
  if (inherits(object, "DLmodel")) {
    
    config <- object$get_config()
    
  } else {
    
    config <- object
    
  }
  
  f_inference <- function(model, 
                          V, 
                          speed = c("faster", "medium", "slower"), 
                          verbose = FALSE,
                          ...) {
    
    num_inputs <- length(V)
    
    stopifnot(inherits(model, "DLmodel"))
    
    .model <- model$get_model()

    stride <- switch(speed, 
                     "slower" = 1,
                     "medium" = (config$output_width + 1) / 2,
                     "faster" = config$output_width)
    
    meanX <- list()
    stdX <- list()
    maxX <- list()
    
    for (input in seq(num_inputs)) {
      
      model$log("INFO", message = "Computing input image statistics.")
      
      if (config$scale %in% c("mean", "z", "meanmax")) meanX[[input]] <- mean(as.vector(V[[input]]))
      if (config$scale %in% "z") stdX[[input]] <- sd(as.vector(V[[input]]))
      if (config$scale %in% c("max", "meanmax")) maxX[[input]] <- max(as.vector(V[[input]]))
      
      if (config$is_autoencoder & !is.null(config$remap_classes)) {
        
        V[[input]] <- map_ids(image = V[[input]], config$remap_classes)
        
      }
      
    }
    
    V0 <- V[[1]] * 0 
    V0[seq(from = 1, to = dim(V0)[1], by = stride), 
       seq(from = 1, to = dim(V0)[2], by = stride), 
       seq(from = 1, to = dim(V0)[3], by = stride)] <- 1 
    all_idx <- which(V0 > 0) 
    
    model$log("INFO", message = "Initializing results.")
    
    if ((config$categorize_output) && (config$category_method == "by_class")) {
      
      if (config$only_convolutionals) {
        
        num_classes <- config$num_classes
        
      } else {
        
        num_classes <- config$last_layer$params$num_classes
        
      }
      
      res <- array(0, dim = c(dim(V[[1]]), num_classes))
      
    } else {
      
      res <- V[[1]] * 0
      
    }
    
    counts <- V[[1]] * 0
    last_distance <- V[[1]] * 0 + 3 * config$width ^ 2
    
    # Must define num_windows
    num_windows <- round(unclass(config$memory_limit / 
                                   object.size(vector(mode = "double",
                                                      length = sum(config$num_volumes) * config$width ^ 3 + 
                                                        config$num_features + 
                                                        config$output_width ^ 3))))
    
    num_windows <- round(num_windows / (num_inputs + 2))
    
    model$log("INFO", message = paste0("Number of windows to read is ", num_windows, "."))
    
    sampling_indices <- all_idx
    num_batches <- ceiling(length(sampling_indices) / num_windows)
    if (num_batches > 1)
      batch_idx <- as.numeric(cut(seq_along(sampling_indices), num_batches))
    else
      batch_idx <- rep(1, times = length(sampling_indices))
    
    if (verbose && require(progress)) {
      
      # nocov start
      
      progress <- TRUE 
      pb_infer <- progress_bar$new(format = " Batch :batch/:total [:bar] ETA: :eta . Elapsed: :elapsed", 
                                   total = num_batches, 
                                   clear = FALSE, 
                                   width = 60) 
      
      pb_infer$update(ratio = 0, tokens = list(batch = 0)) 

      # nocov end
      
    } else {
      
      progress <- FALSE
      
    }
    
    for (batch in seq(num_batches)) {
      
      if (verbose && !progress)
        message("Batch ", batch, " out of ", num_batches) # nocov
      
      model$log("INFO", message = paste0("Start of batch no. ", batch, "."))
      
      idx <- sampling_indices[batch_idx == batch]
      coords <- idx %>% arrayInd(.dim = dim(V[[1]]))
      
      x <- coords[, 1] - 1
      y <- coords[, 2] - 1
      z <- coords[, 3] - 1
      
      X_vol <- list()
      
      model$log("INFO", message = "Reading inputs.")
      
      for (input in seq(num_inputs)) {
        
        X <- get_windows_at(V[[input]], config$width, x, y, z)
        
        X_coords <- X[, 1:3]
        X_coords[, 1] <- X_coords[, 1] / dim(V[[input]])[1]
        X_coords[, 2] <- X_coords[, 2] / dim(V[[input]])[2]
        X_coords[, 3] <- X_coords[, 3] / dim(V[[input]])[3]
        
        X_vol[[input]] <- X[, -c(1:3)]
        
        if (num_windows == 1) {
          
          dim(X_vol[[input]]) <- c(1, length(X_vol[[input]]))
          
        }
        
        if (config$only_convolutionals)
          X_vol[[input]] <- array(X_vol[[input]], dim = c(length(idx), config$width, config$width, config$width, 1))
        
        
        if (config$is_autoencoder) {
          
          if (config$categorize_input) {
            
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
      
      inputs <- switch(config$path[1],
                       
                       "volumes" = X_vol,
                       
                       "both" = c(list(X_coords), X_vol),
                       
                       "features" = X_coords
                       
                       )
      

      # Available memory is the memory limit minus the memory reserved for the parameters in the model
      available_memory <- config$memory_limit - object.size(vector(mode = "double", length = model$get_model()$count_params()))
      
      # Get the maximum number of objects that fit into the memory limit.
      batch_size <- as.integer(available_memory / 
                                 object.size(vector(mode = "double", length = sum(.model %>% model_units()))))
      
      model$log("INFO", message = paste0("batch_size is ", batch_size, "."))
      
      output <- .model$predict(x = inputs, batch_size = as.integer(batch_size))
      
      model$log("INFO", message = "Writing output to correct fomat.")
      
      if (config$only_convolutionals) {
        
        if (config$categorize_output) {
          
          num_classes <- config$num_classes
          
          if (config$category_method == "simple") {
            
            # Remove the class dimension
            new_output <- array(0, dim = dim(output)[-5])
            n_batch <- dim(output)[1]
            
            for (id in seq(n_batch)) {
              
              new_output[id, , , ] <- which_max(output[id, , , , ])
              
            }
            
            if (config$output_width > 1) {
              
              results_to_volume_label_with_distance(V = new_output, 
                                                    width = config$output_width,
                                                    res = res, 
                                                    last_distance = last_distance,  
                                                    x = x, 
                                                    y = y, 
                                                    z = z)
              
            } else {
              
              res[idx] <- new_output
              
            }
            
          } else {
            
            for (k in seq(num_classes)) {
              
              new_output <- output[ , , , , k]
              
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
        
      } else {
        
        if (config$categorize_output) {
          
          # Should be a categorical layer
          if (config$last_layer$type == "categorical") {
            
            num_classes <- config$last_layer$params$num_classes
            units <- config$last_layer$params$units
            
            if (config$multioutput)
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
        
      }
      
      
      if (verbose && progress) {
        
        pb_infer$tick(tokens = list(batch = batch)) # nocov
        
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
        
      } else {
        
        res[which_to_divide] <- res[which_to_divide] / counts[which_to_divide]
        
      }
      
    }
    
    # Normalize sum of probabilities
    if (config$categorize_output & config$category_method == "by_class") {
      
      model$log("INFO", message = "Normalizing output.")
      
      total_prob <- 0 * res[, , , 1]
      
      for (k in seq(num_classes)) {
        
        total_prob <- total_prob + res[, , , k]
        
      }
      
      for (k in seq(num_classes)) {
        
        res[, , , k] <- res[, , , k] / total_prob
        
      }
      
    }
    
    
    if ((!config$categorize_output) | (config$categorize_output & config$category_method == "by_class"))
      if (!is.null(config$regularize)) {
        
        model$log("INFO", message = "Smoothing output.")
        
        res <- smooth_by_gaussian_kernel(image = res, 
                                         kernel_sigma = config$regularize$sigma, 
                                         kernel_width = config$regularize$width)
        
      }
    
    if ((config$categorize_output) & (config$category_method == "by_class")) {
      
      model$log("INFO", message = "Categorizing output."
                )
      res <- which_max(res)
      
    }
    
    if (config$categorize_output) {
      
      model$log("DEBUG", message = "Remapping classes to original indices.")
      
      res <- map_ids(image = res, remap_classes = config$remap_classes, invert = TRUE)
      
    }
    
    return(res)
    
  }
  
  if (inherits(object, "DLmodel")) {
    
    object$log("INFO", message = "Inference function created.")
    
  }
  
  return(f_inference)
  
}
