#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param config            (name) PARAM_DESCRIPTION
#' @param x_files           (name) PARAM_DESCRIPTION
#' @param y_files           (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param mode              (call) PARAM_DESCRIPTION, Default: c("sampling", "all")
#' @param num_windows       (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param max_sub_epochs    (numeric) PARAM_DESCRIPTION, Default: 5
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso 
#'  \code{\link[keras]{to_categorical}}
#' @export 
#' @importFrom keras to_categorical
create_generator_from_config <- function(config, 
                                         x_files, 
                                         y_files = NULL,
                                         mode = c("sampling", "all"),
                                         num_windows = NULL,
                                         max_sub_epochs = 5) {
  
  if ("DLconfig" %in% class(config)) {
    
    num_inputs <- length(config$vol_layers)
    
    mode <- mode[1]
    radius <- 0.5 * (config$width + 1)
    
    if (mode == "all") {
      
      stride <- radius
      
    } else {
      
      stride <- 1
      
    }
    
    if (is.null(num_windows)) {
      
      num_windows <- round(unclass(config$memory_limit / 
                                     object.size(vector(mode = "double",
                                                        length = config$width ^ 3 + 
                                                          config$num_features + config$output_width ^ 3))))
      
      num_windows <- round(num_windows / (num_inputs + 2))
      
      message("Set number of windows to ", num_windows)
      
    }
    
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
      
      Vx[[input]] <- read_nifti_to_array(x_files[[input]][1])
      if (config$scale %in% c("mean", "z", "meanmax")) meanX[[input]] <- mean(as.vector(Vx[[input]]))
      if (config$scale %in% "z") stdX[[input]] <- sd(as.vector(Vx[[input]]))
      if (config$scale %in% c("max", "meanmax")) maxX[[input]] <- max(as.vector(Vx[[input]]))
      
    }
    
    Vy <- read_nifti_to_array(y_files[1])
    
    if (config$scale_y %in% c("mean", "z", "meanmax")) meanY <- mean(as.vector(Vy))
    if (config$scale_y %in% "z") stdY <- sd(as.vector(Vy))
    if (config$scale_y %in% c("max", "meanmax")) maxY <- max(as.vector(Vy))
    
    V0 <- Vy * 0
    V0[seq(from = radius, to = dim(V0)[1] - (radius + 1), by = stride),
       seq(from = radius, to = dim(V0)[2] - (radius + 1), by = stride),
       seq(from = radius, to = dim(V0)[3] - (radius + 1), by = stride)] <- 1
    all_idx <- which(V0 > 0)
    
    if (mode == "all") {
      
      sampling_indices <- all_idx
      message("Number of actual windows: ", length(sampling_indices))
      
      num_batches <- ceiling(length(sampling_indices) / num_windows)
      if (num_batches > 1)
        batch_idx <- as.numeric(cut(seq_along(sampling_indices), num_batches))
      else
        batch_idx <- rep(1, times = length(sampling_indices))
      
    } else {
      
      sampling_indices <- sample(all_idx, length(all_idx))
      
      if (!is.null(config$class_balance) & !is.null(config$y_label)) {
        
        Vy[!(Vy %in% config$y_label)] <- 0
        
        if (config$class_balance == "extensive") {

          balanced_classes <- sample(c(0, config$y_label), size = length(all_idx), replace = TRUE)
          sampling_indices <- rep(0, length(balanced_classes))
          
          for (class in c(0, config$y_label)) {
            
            idx_for_class <- which(balanced_classes == class)
            idx_in_img <- intersect(which(Vy == class), all_idx)
            
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
          
        } else {
          
          which_idx <- which(Vy %in% config$y_label)
          not_idx <- setdiff(all_idx, which_idx)
          
          if (length(not_idx) > length(which_idx)) {
            
            which_idx <- sample(which_idx, size = length(not_idx), replace = TRUE)
            
          } else {
            
            which_idx <- sample(which_idx, size = length(not_idx))
            
          }
          
          sampling_indices <- c(which_idx, not_idx)
          sampling_indices <- sample(sampling_indices, size = length(sampling_indices))
          
        }
        
      }
      
      num_batches <- ceiling(length(sampling_indices) / num_windows)
      if (num_batches > 1)
        batch_idx <- as.numeric(cut(seq_along(sampling_indices), num_batches))
      else
        batch_idx <- rep(1, times = length(sampling_indices))
      
    }
    
    message("Number of batches per volume: ", num_batches)
    
    max_epochs <- min(c(num_batches, max_sub_epochs))
    
    f_generator <- function() {
      
      sub_epoch <<- sub_epoch + 1
      
      print(paste0("Subepoch ", sub_epoch))
      
      if (sub_epoch > max_epochs) {
        
        sub_epoch <<- 1
        
        next_file <<- next_file + 1
        print(paste0("Next_file ", next_file))
        
        
        if (next_file > length(x_files[[1]])) 
          next_file <<- 1
        
        for (input in seq(num_inputs)) {
          
          Vx[[input]] <<- read_nifti_to_array(x_files[[input]][next_file])
          if (config$scale %in% c("mean", "z", "meanmax")) meanX[[input]] <<- mean(as.vector(Vx[[input]]))
          if (config$scale %in% "z") stdX[[input]] <<- sd(as.vector(Vx[[input]]))
          if (config$scale %in% c("max", "meanmax")) maxX[[input]] <<- max(as.vector(Vx[[input]]))
          
        }
        
        Vy <- read_nifti_to_array(y_files[next_file])
        
        if (config$scale_y %in% c("mean", "z", "meanmax")) meanY <<- mean(as.vector(Vy))
        if (config$scale_y %in% "z") stdY <<- sd(as.vector(Vy))
        if (config$scale_y %in% c("max", "meanmax")) maxY <<- max(as.vector(Vy))
        
        if (mode == "sampling") {
          
          sampling_indices <- sample(all_idx, length(all_idx))
          
          if (!is.null(config$class_balance) & !is.null(config$y_label)) {
            
            Vy[!(Vy %in% config$y_label)] <- 0
            
            if (config$class_balance == "extensive") {
              
              balanced_classes <- sample(c(0, config$y_label), size = length(all_idx), replace = TRUE)
              sampling_indices <- rep(0, length(balanced_classes))
              
              for (class in c(0, config$y_label)) {
                
                idx_for_class <- which(balanced_classes == class)
                idx_in_img <- intersect(which(Vy == class), all_idx)
                
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
              
            } else {
              
              which_idx <- which(Vy %in% config$y_label)
              not_idx <- setdiff(all_idx, which_idx)
              
              if (length(not_idx) > length(which_idx)) {
                
                which_idx <- sample(which_idx, size = length(not_idx), replace = TRUE)
                
              } else {
                
                which_idx <- sample(which_idx, size = length(not_idx))
                
              }
              
              sampling_indices <- c(which_idx, not_idx)
              sampling_indices <- sample(sampling_indices, size = length(sampling_indices))
              
            }
            
          }
          
          num_batches <- ceiling(length(sampling_indices) / num_windows)
          if (num_batches > 1)
            batch_idx <- as.numeric(cut(seq_along(sampling_indices), num_batches))
          else
            batch_idx <- rep(1, times = length(sampling_indices))
          
        } else {
          
          # mode == "all"
          sampling_indices <- sample(sampling_indices, size = length(sampling_indices))
          
        }
        
      }
      
      idx <- sampling_indices[batch_idx == sub_epoch]
      
      coords <- idx %>% arrayInd(.dim = dim(Vy))
      x <- coords[, 1] - 1
      y <- coords[, 2] - 1
      z <- coords[, 3] - 1
      
      # str(apply(coords, 2, range))
      # print("Reading X")
      
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
      
      if (any(dim(Vy) != dim(Vx[[input]]))) {
        
        Vy_coords <- transform_coords(x = x, y = y, z = z, Vx = Vx[[input]], Vy = Vy)
        
        x_ <- round(Vy_coords$x)
        y_ <- round(Vy_coords$y)
        z_ <- round(Vy_coords$z)
        
      } else {
        
        x_ <- x
        y_ <- y
        z_ <- z
        
      }
      
      # print("Reading Y")
      Y <- get_windows_at(Vy, config$output_width, x_, y_, z_)
      Y <- Y[, -c(1:3)]
      
      if (!is.null(config$y_label)) {
        
        Y[!(Y %in% config$y_label)] <- 0
        
      }
      
      if (!is.null(config$remap_classes)) {
        
        s <- config$remap_classes$source
        t <- config$remap_classes$target
        
        Y_ <- Y
        
        for (i in seq_along(s)) {
          
          Y_[Y == s[i]] <- t[i]
          
        }
        Y <- Y_
        
      }
      
      if (config$categorize_output) {
        
        Y2 <- keras::to_categorical(Y, num_classes = config$num_classes)
        
        Y <- t(matrix(t(Y2), nrow = config$output_width ^ 3 * config$num_classes))
        
        x <- c(list(X_coords), X_vol)
        
        return(list(x, Y))
        
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
      
      x <- c(list(X_coords), X_vol)
      
      return(list(x, Y))
      
    }
    
    return(list(generator = f_generator, 
                num_windows = num_windows,
                max_sub_epochs = max_epochs,
                num_files = length(y_files)))
    
  } else {
    
    return(NULL)
    
  }
  
}
