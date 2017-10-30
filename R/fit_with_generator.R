#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param .model               (name) PARAM_DESCRIPTION
#' @param generator            (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param steps_per_epoch      (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param train_config         (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param epochs               (numeric) PARAM_DESCRIPTION, Default: 10
#' @param validation_data      (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param validation_steps     (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param validation_config    (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param keep_best            (logical) PARAM_DESCRIPTION, Default: TRUE
#' @param ...                  (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso 
#'  \code{\link[keras]{callback_lambda}}
#' @export 
#' @importFrom keras callback_lambda
#' @import keras
#' @import progress
fit_with_generator <- function(.model,
                               generator = NULL,
                               steps_per_epoch = NULL,
                               train_config = NULL,
                               epochs = 10,
                               validation_data = NULL,
                               validation_steps = NULL, 
                               validation_config = NULL,
                               keep_best = TRUE,
                               ...) {
  
  require(keras)
  
  extra_args <- list(...)
  
  if (is.null(train_config) & is.null(generator) & is.null(steps_per_epoch)) 
    stop("No training configuration/generator provided.")
  
  if (!is.null(train_config)) {
    
    generator <- train_config$generator
    steps_per_epoch <- train_config$max_sub_epochs * train_config$num_files
    batch_size <- train_config$num_windows / 2
    
  } else {
    
    batch_size <- 10000
    
  }
  
  if (is.null(validation_config) & is.null(validation_data) & is.null(validation_steps)) 
    warning("No validation configuration/generator provided.")
  
  if (!is.null(validation_config)) {
    
    validation_data <- validation_config$generator
    validation_steps <- validation_config$max_sub_epochs * validation_config$num_files

  }
  
  path <- NULL
  prefix <- NULL
  
  if (!is.null(extra_args$batch_size)) batch_size <- extra_args$batch_size
  
  
  if (keep_best & ("path" %in% names(extra_args))) path <- extra_args$path
  if (keep_best & ("prefix" %in% names(extra_args))) prefix <- extra_args$prefix
  
  extra_args["path"] <- NULL
  extra_args["prefix"] <- NULL
  
  if (keep_best & is.null(path))  
    model_path <- tempdir()
  else 
    model_path <- path
  
  if (keep_best & is.null(prefix))
    model_prefix <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
  else
    model_prefix <- prefix
  
  if ("DLmodel" %in% class(.model)) {
    
    model <- .model$model
    
    if ("best_loss" %in% names(.model)) {
      
      best_validation_loss <- .model$best_loss
      
    } else {
      
      best_validation_loss <- Inf
      
    }
    
  } else {
    
    stop("Not a DLmodel object")

  }
  
  epoch <- 0
  
  if (keep_best) {
    
    message("Model saved in ", file.path(model_path, model_prefix))
    
    dir.create(file.path(model_path, model_prefix), recursive = TRUE, showWarnings = FALSE)
    
    .model %>% save_model(path = model_path, prefix = model_prefix, 
                                       comment = paste0(timestamp(quiet = TRUE), ":", " Initial state.\n"))
    
    on.exit({
      
      if (epoch > 1) {
        
        message("Loading previous best model, with loss:", best_validation_loss)
        
        weights_filename <- file.path(model_path, model_prefix, paste0(model_prefix, "_weights.hdf5"))
        
        .model$model %>% load_model_weights_hdf5(filepath = weights_filename)
        # file.path(model_pathmodel_filename %>% load_model_hdf5()
        
        .model$best_loss <- best_validation_loss
        
      }
      
    })
    
  }
  
  if (require(progress)) {
    
    progress <- TRUE
    pb_epochs <- progress_bar$new(format = " Epoch :epoch/:total [:bar] ETA: :eta . Elapsed: :elapsed",
                                  total = epochs,
                                  clear = FALSE,
                                  width = 60)
    
    pb_epochs$update(ratio = 0, tokens = list(epoch = 0))
    
  } else {
    
    progress <- FALSE
    
  }
  
  last_loss <- Inf
  
  my_callback <- keras::callback_lambda(
    on_batch_end = function(batch, logs = NULL) {
      last_loss <<- logs$loss
    }
  )
  
  
  for (epoch in seq(epochs)) {
    
    message("\nEpoch ", epoch,  "/", epochs)
    
    if (!progress) {

      message("==========")
      message("Start Training")
      
    }
    
    training_loss <- 0
    
    if (progress) {
      
      pb_subepoch <- progress_bar$new(format = paste0("   Training Subepoch :subepoch/:total ",
                                                      "[:bar] ETA: :eta . Elapsed: :elapsed. ",
                                                      "Current loss = :loss"),
                                      total = steps_per_epoch,
                                      clear = FALSE,
                                      width = 100)
      
      pb_subepoch$update(ratio = 0, tokens = list(subepoch = 0, loss = last_loss)) 
      
    }
    
    for (step in seq(steps_per_epoch)) {
      
      # Get Training data
      data <- generator()
      
      if (length(data) > 2) {
        # With sample weight
        
        model %>% fit(
          x = data[[1]], y = data[[2]],
          epochs = epoch, 
          verbose = 0, 
          batch_size = batch_size,
          sample_weight = to_numpy_array(data[[3]]),
          initial_epoch = epoch - 1, 
          callbacks = my_callback)#,
          # ...)
        
      } else {
        # No sample weight
        
        model %>% fit(
          x = data[[1]], y = data[[2]],
          epochs = epoch, 
          verbose = 0, 
          batch_size = batch_size,
          initial_epoch = epoch - 1, 
          callbacks = my_callback) #,
          # ...)
        
      }
      
      if (progress)
        pb_subepoch$tick(tokens = list(subepoch = step, 
                                       loss = sprintf("%.5f", last_loss)))
      
    }
    
    
    training_loss <- last_loss
    
    if (!progress) 
      message("Training loss: ", training_loss)
    
    # We have validation data
    if (!is.null(validation_data)) {
      
      if (!progress)
        message("   Start Validation")
      
      loss <- 0
      loss_acc <- 0
      
      if (is.function(validation_data)) {
        
        # It is a generator
        
        if (progress) {
          
          pb_subepoch_val <- progress_bar$new(format = paste0("   Validating Subepoch :subepoch/:total ",
                                                              "[:bar] ETA: :eta . Elapsed: :elapsed. ",
                                                              "Validation loss = :loss"),
                                              total = validation_steps,
                                              clear = FALSE,
                                              width = 100)
          
          pb_subepoch_val$update(ratio = 0, tokens = list(subepoch = 0, loss = Inf))
          
        }
        
        
        for (val_steps in seq(validation_steps)) {
          
          
          test_data <- validation_data()
          loss <- model %>% evaluate(x = test_data[[1]],
                                     y = test_data[[2]],
                                     batch_size = batch_size,
                                     verbose = 0)
          if (is.list(loss)) loss <- loss$loss
          loss_acc <- loss_acc + loss
          
          if (progress)
            pb_subepoch_val$tick(tokens = list(subepoch = val_steps, 
                                               loss = sprintf("%.5f", loss)))
          
        }
        
        loss_acc <- loss_acc / validation_steps
        
      } else {
        
        # It is data
        loss_acc <- model %>% evaluate(x = validation_data[[1]],
                                       y = validation_data[[2]])
        
      }
      
      if (keep_best & (loss_acc < best_validation_loss)) {
        
        best_validation_loss <- loss_acc
        .model %>% save_model(path = model_path, prefix = model_prefix, 
                              comment = paste0(timestamp(quiet = TRUE), ":", " Model with loss: ", loss_acc, "\n"))
        message("New best model found with loss:", loss_acc)
        
      }
      
      if (!progress)
        message("\nValidation loss: ", loss_acc)
      
    }
    
    
    if (progress) 
      pb_epochs$tick(tokens = list(epoch = epoch))
    
  }
  
  if (keep_best) {
    
    message("Loading previous best model, with loss:", best_validation_loss)
    
    weights_filename <- file.path(model_path, model_prefix, paste0(model_prefix, "_weights.hdf5"))
    
    .model$model %>% load_model_weights_hdf5(filepath = weights_filename)
    # file.path(model_pathmodel_filename %>% load_model_hdf5()
    
    .model$best_loss <- best_validation_loss

  }
  
}
