#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param model                    (name) PARAM_DESCRIPTION
#' @param x_files                  (name) PARAM_DESCRIPTION
#' @param y_files                  (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param mode                     (call) PARAM_DESCRIPTION, Default: c("sampling", "all")
#' @param target_windows_per_file  (numeric) PARAM_DESCRIPTION, Default: 1024
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso 
#'  \code{\link[keras]{to_categorical}}
#' @export 
#' @importFrom keras to_categorical
create_generator <- function(model, 
                             x_files, 
                             y_files = NULL,
                             mode = "sampling",
                             target_windows_per_file = 1024,
                             verbose = FALSE) {
  
  stopifnot(inherits(model, "DLmodel"))
  
  config <- model$get_config()
  
  require(utils4ni)
  # require(tictoc)
  
  # tic("Initialization")
  
  num_inputs <- length(config$vol_layers)
  
  mode <- mode[1]
  radius <- floor(0.5 * (config$width + 1))
  
  stride <- ifelse(mode == "all", radius, 1)
  
  num_windows <- model$check_memory() # %>% compute_batch_size()
  batches_per_file <- ceiling(target_windows_per_file / num_windows)
  
  if (verbose) {
    
    # nocov start
    
    message("Number of windows per batch is set to ", num_windows) 
    message("Will use ", batches_per_file, 
            " batches to achieve ", batches_per_file * num_windows, 
            " windows extracted per each image.") 
    
    # nocov end
    
  }
  
  next_file <- 1
  sub_epoch <- 0
  
  if (config$is_autoencoder) {
    
    y_files <- x_files[[1]]
    config$scale_y <- config$scale
    
  }
  
  Vx <- list()
  statsX <- list()
  
  if (config$use_data_augmentation) {
    
    M <- random_transformation_matrix(scale_range = config$augment_scales, 
                                      rotation_range = config$augment_rotations, 
                                      translation_range = config$augment_translations)
    
  } else {
    
    M <- identity_matrix()
    
  }
  
  for (input in seq(num_inputs)) {
    
    # cat("Reading", x_files[[input]][1], "\n")
    
    tmpVx <- read_nifti_to_array(x_files[[input]][next_file])
    
    Vx[[input]] <- apply_image_augmentation(tmpVx, M = M, type = config$input_types[input])
    
    statsX[[input]] <- get_image_stats(Vx[[input]])
    
    if (config$is_autoencoder & !is.null(config$remap_classes)) {
      
      Vx[[input]] <- map_ids_cpp(image = Vx[[input]], config$remap_classes)
      
    }
    
  }
  
  # cat("Reading", y_files[1], "\n")
  
  tmpVy <- read_nifti_to_array(y_files[next_file])
  
  output_type <- "continuous"
  if (config$categorize_output) output_type <- "categorical"
  
  Vy <- apply_image_augmentation(tmpVy, M = M, type = output_type)
  
  statsY <- get_image_stats(Vy)
  
  if (!is.null(config$class_balance) & !is.null(config$y_label)) {
    
    Vy <- map_ids_cpp(image = Vy, remap_classes = config$remap_classes)
    unique_labels <- unique(c(0, config$remap_classes$target, config$remap_classes$remaining))
    
  } else {
    
    unique_labels <- 0
    
  }
  
  sample <- utils4ni::get_sample_indices(Vy = Vy,
                                         num_windows = num_windows,
                                         batches_per_file = batches_per_file,
                                         stride = stride,
                                         mode = mode,
                                         class_balance = config$class_balance,
                                         unique_labels = unique_labels,
                                         verbose = verbose)
  
  sampling_indices <- sample$sampling_indices
  batch_idx <- sample$batch_idx
  num_batches <- sample$num_batches
  max_epochs <- sample$max_epochs
  
  if (verbose)
    message("Number of batches per volume: ", num_batches) # nocov
  
  max_epochs <- min(c(num_batches, batches_per_file))
  
  # toc()
  
  f_generator <- function() {
    
    # tic("Total")
    
    sub_epoch <<- sub_epoch + 1
    
    # cat("Subepoch", sub_epoch, "\n")
    
    if (sub_epoch > max_epochs) {
      
      # tic("Reinitialization")
      
      sub_epoch <<- 1
      
      next_file <<- next_file + 1
      # print(paste0("Next_file ", next_file))
      
      if (next_file > length(x_files[[1]])) 
        next_file <<- 1
      
      # tic("Reading X")
      Vx <<- list()

      if (config$use_data_augmentation) {
        
        M <<- random_transformation_matrix(scale_range = config$augment_scales, 
                                           rotation_range = config$augment_rotations, 
                                           translation_range = config$augment_translations)
        
      } else {
        
        M <- identity_matrix()
        
      }
      
      for (input in seq(num_inputs)) {
        
        # cat("Reading", x_files[[input]][1], "\n")
        
        tmpVx <- read_nifti_to_array(x_files[[input]][next_file])
        
        Vx[[input]] <<- apply_image_augmentation(tmpVx, M = M, type = config$input_types[input])
        
        statsX[[input]] <<- get_image_stats(Vx[[input]])
        
        if (config$is_autoencoder & !is.null(config$remap_classes)) {
          
          Vx[[input]] <<- map_ids_cpp(image = Vx[[input]], config$remap_classes)
          
        }
        
      }
      
      # cat("Reading", y_files[1], "\n")
      
      tmpVy <- read_nifti_to_array(y_files[next_file])
      
      output_type <- "continuous"
      if (config$categorize_output) output_type <- "categorical"
      
      Vy <<- apply_image_augmentation(tmpVy, M = M, type = output_type)
      
      statsY <<- get_image_stats(Vy)
      # toc()
      # 
      
      if (!is.null(config$class_balance) & !is.null(config$y_label)) {
        
        Vy <<- map_ids_cpp(image = Vy, remap_classes = config$remap_classes)
        unique_labels <- unique(c(0, config$remap_classes$target, config$remap_classes$remaining))
        
      } else {
        
        unique_labels <- 0
        
      }
      
      sample <- utils4ni::get_sample_indices(Vy = Vy,
                                             num_windows = num_windows,
                                             batches_per_file = batches_per_file,
                                             stride = stride,
                                             mode = mode,
                                             class_balance = config$class_balance,
                                             unique_labels = unique_labels,
                                             verbose = verbose)
      
      sampling_indices <<- sample$sampling_indices
      batch_idx <<- sample$batch_idx
      num_batches <- sample$num_batches
      max_epochs <- sample$max_epochs
      
    }
    
    # tic("Coordinates")
    # print(batch_idx)
    idx <- sampling_indices[which(batch_idx == sub_epoch)]
    
    # print(idx)
    
    coords <- idx %>% arrayInd(.dim = dim(Vy))
    
    x <- coords[, 1] - 1
    y <- coords[, 2] - 1
    z <- coords[, 3] - 1
    
    # toc()
    
    # tic("Reading inputs")
    X_vol <- list()
    
    X_coords <- cbind(x, y, z)
    X_coords[, 1] <- X_coords[, 1] / dim(Vx[[input]])[1]
    X_coords[, 2] <- X_coords[, 2] / dim(Vx[[input]])[2]
    X_coords[, 3] <- X_coords[, 3] / dim(Vx[[input]])[3]
    
    
    for (input in seq(num_inputs)) {
      
      nv <- 1
      if (length(dim(Vx[[input]])) > 3) {
        
        nv <- dim(Vx[[input]])[4]
        
      }
      
      X <- get_windows_at(Vx[[input]], config$width, x, y, z)
      
      X_vol[[input]] <- X[, -c(1:3)]
      
      if (num_windows == 1) {
        
        dim(X_vol[[input]]) <- c(1, length(X_vol[[input]]))
        
      }
      
      if (config$only_convolutionals)
        X_vol[[input]] <- array(X_vol[[input]], dim = c(length(idx), config$width, config$width, config$width, nv))
      
      
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
                   "mean" = X_vol[[input]] <- X_vol[[input]] - statsX[[input]]$mean,
                   "z"    = X_vol[[input]] <- (X_vol[[input]] - statsX[[input]]$mean) / statsX[[input]]$sd,
                   "max"  = X_vol[[input]] <- X_vol[[input]] / statsX[[input]]$max,
                   "meanmax" = X_vol[[input]] <- (X_vol[[input]] - statsX[[input]]$mean) / (statsX[[input]]$max - statsX[[input]]$mean))
            
          }
          
        }
        
      } else {
        
        switch(config$scale,
               "none" = X_vol[[input]] <- X_vol[[input]],
               "mean" = X_vol[[input]] <- X_vol[[input]] - statsX[[input]]$mean,
               "z"    = X_vol[[input]] <- (X_vol[[input]] - statsX[[input]]$mean) / statsX[[input]]$sd,
               "max"  = X_vol[[input]] <- X_vol[[input]] / statsX[[input]]$max,
               "meanmax" = X_vol[[input]] <- (X_vol[[input]] - statsX[[input]]$mean) / (statsX[[input]]$max - statsX[[input]]$mean))
        
      }
      
      
    }
    
    x_input <- switch(config$path[1],
                      
                      "volumes" = X_vol,
                      
                      "both" = c(list(X_coords), X_vol),
                      
                      "features" = X_coords)
    
    
    # toc()
    # print("After Reading X")
    if (any(dim(Vy) != dim(Vx[[1]]))) {
      
      Vy_coords <- transform_coords(x = x, y = y, z = z, Vx = Vx[[1]], Vy = Vy)
      
      x_ <- round(Vy_coords$x)
      y_ <- round(Vy_coords$y)
      z_ <- round(Vy_coords$z)
      
    } else {
      
      x_ <- x
      y_ <- y
      z_ <- z
      
    }
    
    # tic("Reading outputs")
    
    Y <- get_windows_at(Vy, config$output_width, x_, y_, z_)
    Y <- Y[, -c(1:3)]
    
    # toc()
    
    if (config$only_convolutionals) {
      
      # tic("Transforming for convolutional")
      
      Y <- array(Y, dim = c(length(idx), config$output_width, config$output_width, config$output_width, 1))
      
      if (config$categorize_output) {
        
        Y_new <- to_categorical_volume_cpp(Y[, , , , 1], unique_labels = unique_labels)
        
        if (num_windows == 1) {
          
          dim(Y_new) <- c(1, dim(Y_new))
          
        }
        
        return(list(x_input, Y_new))
        
      }
      
      
    } else {
      
      if (config$categorize_output) {
        
        Y2 <- keras::to_categorical(Y, num_classes = config$num_classes)
        
        Y <- t(matrix(t(Y2), nrow = config$output_width ^ 3 * config$num_classes))
        
        if (config$multioutput) {
          
          # cat("Multioutput\n")
          
          Y_list <- list()
          for (i in seq(config$output_width ^ 3)) {
            
            Y_list[[i]] <- Y[ , 1:config$num_classes]
            Y <- Y[, -c(1:config$num_classes)]
            
          }
          
          Y <- Y_list
          
        }
        
        # toc()
        
        return(list(x_input, Y))
        
      }
      
    }
    
    if (!is.null(config$y_label)) {
      
      Y[!(Y %in% config$y_label)] <- 0
      
    }
    
    if (config$scale_y == "none") {
      
      if (config$binarise) {
        
        Y[!(Y %in% config$y_label)] <- -1
        Y[Y %in% config$y_label] <- 1
        
      } 
      
    } else {
      
      switch(config$scale_y,
             "mean" = Y <- Y - statsY$mean,
             "z"    = Y <- (Y - statsY$mean) / statsY$sd,
             "max"  = Y <- Y / statsY$max,
             "meanmax" = Y <- (Y - statsY$mean) / (statsY$max - statsY$mean))
      
      
    }
    
    
    return(list(x_input, Y))
    
    
  }
  
  return(list(generator = f_generator, 
              num_windows = num_windows,
              max_sub_epochs = batches_per_file,
              num_files = length(y_files)))
  
  
  
}
