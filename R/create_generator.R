#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param model                    (name) PARAM_DESCRIPTION
#' @param x_files                  (name) PARAM_DESCRIPTION
#' @param y_files                  (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param mode                     (call) PARAM_DESCRIPTION, Default: c("sampling", "all")
#' @param num_windows              (NULL) PARAM_DESCRIPTION, Default: NULL
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
                             target_windows_per_file = 1024) {
  
  stopifnot(inherits(model, "DLmodel"))
  
  config <- model$get_config()
  
  require(tictoc)
  # tic("Initialization")
  
  num_inputs <- length(config$vol_layers)
  
  mode <- mode[1]
  radius <- 0.5 * (config$width + 1)
  
  stride <- ifelse(mode == "all", radius, 1)
  
  num_windows <- model$check_memory() # %>% compute_batch_size()
  batches_per_file <- ceiling(target_windows_per_file / num_windows)
  message("Number of windows per batch is set to ", num_windows)
  message("Will use ", batches_per_file, " batches to achieve ", batches_per_file * num_windows, " windows extracted per each image.")
  
  next_file <- 1
  sub_epoch <- 0
  
  if (config$is_autoencoder) {
    
    y_files <- x_files[[1]]
    config$scale_y <- config$scale
    
  }
  
  Vx <- list()
  meanX <- list()
  stdX <- list()
  maxX <- list()
  
  for (input in seq(num_inputs)) {
    
    # cat("Reading", x_files[[input]][1], "\n")
    
    Vx[[input]] <- read_nifti_to_array(x_files[[input]][1])
    if (config$scale %in% c("mean", "z", "meanmax")) meanX[[input]] <- mean(as.vector(Vx[[input]]))
    if (config$scale %in% "z") stdX[[input]] <- sd(as.vector(Vx[[input]]))
    if (config$scale %in% c("max", "meanmax")) maxX[[input]] <- max(as.vector(Vx[[input]]))
    
    if (config$is_autoencoder & !is.null(config$remap_classes)) {
      
      Vx[[input]] <- map_ids(image = Vx[[input]], config$remap_classes)
      
    }
    
  }
  
  # cat("Reading", y_files[1], "\n")
  
  Vy <- read_nifti_to_array(y_files[1])
  
  if (config$scale_y %in% c("mean", "z", "meanmax")) meanY <- mean(as.vector(Vy))
  if (config$scale_y %in% "z") stdY <- sd(as.vector(Vy))
  if (config$scale_y %in% c("max", "meanmax")) maxY <- max(as.vector(Vy))
  
  V0 <- Vy * 0 
  V0[seq(from = 1, to = dim(V0)[1], by = stride), 
     seq(from = 1, to = dim(V0)[2], by = stride), 
     seq(from = 1, to = dim(V0)[3], by = stride)] <- 1 
  all_idx <- which(V0 > 0) 
  
  if (mode == "all") {
    
    sampling_indices <- all_idx
    message("Number of actual windows: ", length(sampling_indices))
    
    num_batches <- ceiling(length(sampling_indices) / num_windows)
    max_epochs <- min(c(num_batches, batches_per_file))
    
    if (max_epochs > 1) {
      
      batch_idx <- rep(seq(max_epochs), each = num_windows) 
      batch_idx <- batch_idx[seq_along(sampling_indices)]
      
    } else
      batch_idx <- rep(1, times = length(sampling_indices))
    
  } else {
    
    sampling_indices <- sample(all_idx, length(all_idx))
    
    if (!is.null(config$class_balance) & !is.null(config$y_label)) {
      
      Vy <- map_ids(image = Vy, remap_classes = config$remap_classes)
      unique_labels <- unique(c(0, config$remap_classes$target, config$remap_classes$remaining))
      
      balanced_classes <- sample(unique_labels, size = length(all_idx), replace = TRUE)
      sampling_indices <- rep(0, length(balanced_classes))
      
      for (class in unique_labels) {
        
        idx_for_class <- which(balanced_classes == class)
        idx_in_img <- which(Vy == class)
        
        if (length(idx_in_img) > 0) {
          idx <- sample(idx_in_img, 
                        size = length(idx_for_class), 
                        replace = length(idx_for_class) > length(idx_in_img))
          sampling_indices[idx_for_class] <- idx
          
        }
        
      }
      
      sampling_indices <- sampling_indices[sampling_indices > 0]
      
      if (length(sampling_indices) < length(all_idx))
        sampling_indices <- sample(sampling_indices, 
                                   size = length(all_idx),
                                   replace = TRUE)
      
      
    }
    
    num_batches <- ceiling(length(sampling_indices) / num_windows)
    max_epochs <- min(c(num_batches, batches_per_file))
    
    batch_idx <- rep(seq(max_epochs), each = num_windows) 
    
  }
  
  # print(batch_idx)
  
  message("Number of batches per volume: ", num_batches)
  
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
      meanX <<- list()
      stdX <<- list()
      maxX <<- list()
      
      for (input in seq(num_inputs)) {
        
        # cat("Reading ", x_files[[input]][next_file], "\n")
        Vx[[input]] <<- read_nifti_to_array(x_files[[input]][next_file])
        if (config$scale %in% c("mean", "z", "meanmax")) meanX[[input]] <<- mean(as.vector(Vx[[input]]))
        if (config$scale %in% "z") stdX[[input]] <<- sd(as.vector(Vx[[input]]))
        if (config$scale %in% c("max", "meanmax")) maxX[[input]] <<- max(as.vector(Vx[[input]]))
        
        if (config$is_autoencoder & !is.null(config$remap_classes)) {
          
          Vx[[input]] <- map_ids(image = Vx[[input]], config$remap_classes)
          
        }
        
      }
      
      # toc()
      # cat("Reading ", y_files[next_file], "\n")
      
      # tic("Reading Y")
      Vy <<- read_nifti_to_array(y_files[next_file])
      
      if (config$scale_y %in% c("mean", "z", "meanmax")) meanY <<- mean(as.vector(Vy))
      if (config$scale_y %in% "z") stdY <<- sd(as.vector(Vy))
      if (config$scale_y %in% c("max", "meanmax")) maxY <<- max(as.vector(Vy))
      
      # toc()
      
      # tic("Sampling")
      if (mode == "sampling") {
        
        
        if (!is.null(config$class_balance) & !is.null(config$y_label)) {
          
          # tic("Mapping")
          Vy <<- map_ids(image = Vy, remap_classes = config$remap_classes)
          # toc()
          
          unique_labels <- unique(c(0, config$remap_classes$target, config$remap_classes$remaining))
          
          # tic("Class balancing")
          balanced_classes <- sample(unique_labels, size = length(all_idx), replace = TRUE)
          sampling_indices <- rep(0, length(balanced_classes))
          
          for (class in unique_labels) {
            
            idx_for_class <- which(balanced_classes == class)
            idx_in_img <- which(Vy == class)
            
            if (length(idx_in_img) > 0) {
              idx <- sample(idx_in_img, 
                            size = length(idx_for_class), 
                            replace = length(idx_for_class) > length(idx_in_img))
              sampling_indices[idx_for_class] <- idx
              
            }
            
          }
          
          # toc()
          
          # tic("Resampling")
          sampling_indices <<- sampling_indices[sampling_indices > 0]
          
          if (length(sampling_indices) < length(all_idx))
            sampling_indices <<- sample(sampling_indices, 
                                        size = length(all_idx),
                                        replace = TRUE)
          
          # toc()
          
          
        } else {
          
          # tic("First sample")
          sampling_indices <<- sample(all_idx, length(all_idx))
          # toc()
          
          
        }
        
        # tic("Num batches")
        num_batches <- ceiling(length(sampling_indices) / num_windows)
        batch_idx <<- rep(seq(max_epochs), each = num_windows) 
        
        # toc()
        
      } else {
        
        sampling_indices <<- sample(sampling_indices, size = length(sampling_indices))
        
      }
      
      # toc()
      # toc()
      
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
    
    for (input in seq(num_inputs)) {
      
      X <- get_windows_at(Vx[[input]], config$width, x, y, z)
      X_coords <- X[, 1:3]
      X_coords[, 1] <- X_coords[, 1] / dim(Vx[[input]])[1]
      X_coords[, 2] <- X_coords[, 2] / dim(Vx[[input]])[2]
      X_coords[, 3] <- X_coords[, 3] / dim(Vx[[input]])[3]
      
      X_vol[[input]] <- X[, -c(1:3)]
      
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
                   "meanmax" = X_vol[[input]] <- (X_vol[[input]] - meanX[[input]]) / (maxX[[input]] - meanX[[input]]),
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
             "mean" = Y <- Y - meanY,
             "z"    = Y <- (Y - meanY) / stdY,
             "max"  = Y <- Y / maxY,
             "meanmax" = Y <- (Y - meanY) / (maxY - meanY),
             "110" = Y <- Y - 110)
      
      
    }
    
    
    return(list(x_input, Y))
    
    
  }
  
  return(list(generator = f_generator, 
              num_windows = num_windows,
              max_sub_epochs = batches_per_file,
              num_files = length(y_files)))
  
  
  
}
