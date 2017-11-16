#' @title Fit a Model using Generators
#'
#' @description this function is used to fit or train a \code{DLmodel} by using generator 
#' functions for training and validation data.
#'
#' @param .model               (\code{DLmodel}) The model to train
#' @param generator            (function) The generator function for training data, built by \code{\link{create_generator_from_config}}, Default: NULL
#' @param steps_per_epoch      (integer) Number of steps per training epoch, Default: NULL
#' @param train_config         (list) The training configuration, the output of \code{\link{create_generator_from_config}} when inputs are training files, Default: NULL
#' @param epochs               (numeric) Maximum number of epochs to train, Default: 10
#' @param starting_epoch       (numeric) strating epoch, useful when we want to resume a previous fit, Default: 1
#' @param validation_data      (function or matrix) Data for validation. It can be a generator function, built by \code{\link{create_generator_from_config}}, Default: NULL
#' @param validation_steps     (integer) Number of steps of validation per epoch, Default: NULL
#' @param validation_config    (list) The testing configuration, the output of \code{\link{create_generator_from_config}} when inputs are validation files, Default: NULL
#' @param keep_best            (logical) Should the training always store the best model up-to-date?, Default: TRUE
#' @param verbose              (logical) Provide additional information on training, Default: TRUE
#' @param ...                  extra arguments passed to other functions.
#'
#' @return The trained \code{DLmodel}.
#'
#' @details \code{generator}, \code{steps_per_epoch}, \code{validation_data} and \code{validation_steps} are completely and automatically determined if one uses the \code{train_config} and \code{validation_config} parameters, both being the outputs of \code{\link{create_generator_from_config}}.
#' 
#' Additionally, we can pass this function two arguments: \code{path} and \code{prefix}, the best model will be stored in the corresponding path with the given prefix (usually something indicative or with a timestamp).
#' 
#' @seealso 
#'  \code{\link[keras]{callback_lambda}}
#'  
#' @export 
#' @importFrom keras callback_lambda
#' @import keras
#' @import progress
#' 
fit_with_generator <- function(.model,
                               generator = NULL,
                               steps_per_epoch = NULL,
                               train_config = NULL,
                               epochs = 10,
                               starting_epoch = 1,
                               validation_data = NULL,
                               validation_steps = NULL, 
                               validation_config = NULL,
                               keep_best = TRUE,
                               verbose = TRUE,
                               tensorboard_dir = NULL,
                               ...) {
  
  require(keras)
  
  # Basic input class check
  stopifnot(inherits(.model, "DLmodel"))
  
  # Manage extra arguments
  extra_args <- list(...)
  
  # At least we require the training configuration
  if (is.null(train_config) & is.null(generator) & is.null(steps_per_epoch)) 
    stop("No training configuration/generator provided.")
  
  # If we have a train_config, this define the other variables:
  if (!is.null(train_config)) {
    
    generator <- train_config$generator
    steps_per_epoch <- train_config$max_sub_epochs * train_config$num_files
    batch_size <- floor(train_config$num_windows / 2)
    
  } else {
    
    batch_size <- 10000
    
  }
  
  # If the batch_size is defined explicitly, use the provided by the user
  if (!is.null(extra_args$batch_size)) batch_size <- extra_args$batch_size
  
  
  # If no validation is provided, a warning will be given
  if (is.null(validation_config) & is.null(validation_data) & is.null(validation_steps)) 
    warning("No validation configuration/generator provided.")
  
  # If it is provided, other variables are defined from the configuration.
  if (!is.null(validation_config)) {
    
    validation_data <- validation_config$generator
    validation_steps <- validation_config$max_sub_epochs * validation_config$num_files
    
  }
  
  # Let's take into account where to store the best model
  path <- NULL
  prefix <- NULL
  
  # If path or prefix are provided, use them.
  if (keep_best & ("path" %in% names(extra_args))) path <- extra_args$path
  if (keep_best & ("prefix" %in% names(extra_args))) prefix <- extra_args$prefix
  
  extra_args["path"] <- NULL
  extra_args["prefix"] <- NULL
  
  # If not, use temp folder and file.
  if (keep_best & is.null(path))  
    model_path <- tempdir()
  else 
    model_path <- path
  
  if (keep_best & is.null(prefix))
    model_prefix <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
  else
    model_prefix <- prefix
  
  # Initialize variables for training
  model <- .model$model
  
  # Compute optimal batch size
  batch_size <- .model %>% compute_batch_size()
  
  # If batch_size == 0, there is no possibility of training with the specified memory limit.
  if (batch_size < 1) {
    
    required_memory <- prettyunits::pretty_bytes(unclass(.model %>% model_size() * 4))
    
    # Not enough memory to train even 1 batch at a time
    error_message <- paste0("Not enough memory to train this model. Optimal batch size is 0 for the memory limit: ", 
                            prettyunits::pretty_bytes(.model$hyperparameters$memory_limit), "\n",
                            "This model requires at least ", required_memory, " to be trained.\n",
                            "We suggest to increase this limit by adding: memory_limit = ", required_memory, " to the scheme.\n")
    stop(error_message)
    
  }
  
  cat("Batch size is stablished at:", batch_size, "\n")
  
  
  # Unfreeze learning phase (freeze at the end)
  .model %>% set_trainability(trainability = TRUE)
  
  if ("best_loss" %in% names(.model)) {
    
    best_validation_loss <- .model$best_loss
    
  } else {
    
    best_validation_loss <- Inf
    
  }
  
  epoch <- 0
  
  # Set variables needed to keep always the best model trained so far
  if (keep_best) {
    
    # Path to store the best model
    save_path <- file.path(model_path, model_prefix)
    
    # If it exists, we ask the user for some input: Should we:
    # Overwrite?
    # Load the previous model?
    # Save both?
    # Note that this only works when in interactive mode. If in a script, it will
    # always overwrite.
    if (file.exists(save_path) & interactive()) {
      
      choices <- c("Overwrite", "Load previous", "Save both")
      title <- "Already existing model. Choose an action:"
      chosen_output <- utils::select.list(choices = choices, title = title)
      
      # Perform the required action
      switch(chosen_output,
             
             "Overwrite" = {
               
               .model %>% save_model(path = model_path, prefix = model_prefix, 
                                     comment = paste0(timestamp(quiet = TRUE), ":", " New initial state.\n"))
               
               saveRDS(best_validation_loss, file = file.path(model_path, model_prefix, paste0(model_prefix, "_loss.rds")))
               
             },
             
             "Load previous" = {
               
               .model <- load_model(path = model_path, prefix = model_prefix)
               
               if (file.exists(file.path(model_path, model_prefix, paste0(model_prefix, "_loss.rds"))))
                 best_validation_loss <- readRDS(file = file.path(model_path, model_prefix, paste0(model_prefix, "_loss.rds")))
               
               if (verbose)
                 message("Previous model loaded.")
               
             },
             
             "Save both" = {
               
               model_prefix <- c(model_prefix, "_other")
               save_path <- file.path(model_path, model_prefix)
               
               dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
               
               .model %>% save_model(path = model_path, prefix = model_prefix, 
                                     comment = paste0(timestamp(quiet = TRUE), ":", " Initial state.\n"))
               
               saveRDS(best_validation_loss, file = file.path(model_path, model_prefix, paste0(model_prefix, "_loss.rds")))
               
             })
      
      if (verbose)
        message("Model saved in ", save_path)
      
      
    } else {
      
      # Overwrite if in a script or new training 
      
      dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
      
      .model %>% save_model(path = model_path, prefix = model_prefix, 
                            comment = paste0(timestamp(quiet = TRUE), ":", " Initial state.\n"))
      
      if (verbose)
        message("Model saved in ", save_path)
      
    }
    
    # Define the action to perform when exiting this training:
    # If we have trained for more than one epoch, load the best
    # model up-to-date.
    # This works even when the user interrupts training.
    on.exit({
      
      if (epoch > 1) {
        
        message("Loading previous best model, with loss:", best_validation_loss)
        
        weights_filename <- file.path(model_path, model_prefix, paste0(model_prefix, "_weights.hdf5"))
        
        .model$model %>% load_model_weights_hdf5(filepath = weights_filename)
        
        # Unfreeze learning phase (freeze at the end)
        .model$model %>% set_trainability(trainability = TRUE)
        
        .model$best_loss <- best_validation_loss
        
      }
      
    })
    
  }
  
  # Package progress is useful to make graphical progress bars.
  if (verbose) {
    
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
    
  }
  
  # Initialize the callbacks for the training
  last_loss <- Inf
  
  my_callback <- keras::callback_lambda(
    on_batch_end = function(batch, logs = NULL) {
      last_loss <<- logs$loss
    }
  )
  
  if (!is.null(tensorboard_dir)) {
    
    tensorboard_callback <- callback_tensorboard(log_dir = tensorboard_dir)
    my_callback <- c(my_callback, tensorboard_callback)
    
    message("Initializing Tensorboard.")
    tensorboard(log_dir = tensorboard_dir)
    
  }
  
  # Select epochs to train, useful to resume previous trainings.
  training_epochs <- seq(epochs)
  if (starting_epoch > 1) 
    training_epochs <- setdiff(training_epochs, seq(starting_epoch - 1))
  
  # For each training epoch
  for (epoch in training_epochs) {
    
    # Information (progress bar) for epochs.
    if (verbose) {
      
      message("\nEpoch ", epoch,  "/", epochs)
      
      if (!progress) {
        
        message("==========")
        message("Start Training")
        
      }
      
    }
    
    training_loss <- 0
    
    # Information (progress bar) for subepochs
    if (verbose) {
      
      if (progress) {
        
        pb_subepoch <- progress_bar$new(format = paste0("   Training Subepoch :subepoch/:total ",
                                                        "[:bar] ETA: :eta . Elapsed: :elapsed. ",
                                                        "Current loss = :loss"),
                                        total = steps_per_epoch,
                                        clear = FALSE,
                                        width = 100)
        
        pb_subepoch$update(ratio = 0, tokens = list(subepoch = 0, loss = last_loss)) 
        
      }
      
    }
    
    # For each subepoch
    for (step in seq(steps_per_epoch)) {
      
      # Get Training datausing the generator
      # data has 2 or 3 components:
      # The first is always the input samples
      # The second is always the desired outputs
      # A possible third indicates sample weights.
      data <- generator()
      
      num_outputs <- ifelse(is.list(data[[2]]), length(data[[2]]), 1)
      
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
      
      last_loss <- last_loss / num_outputs
      
      if (verbose) {
        
        if (progress)
          pb_subepoch$tick(tokens = list(subepoch = step, 
                                         loss = sprintf("%.5f", last_loss)))
        
      }
      
    }
    
    # Loss obtained after this training epoch
    training_loss <- last_loss
    
    if (verbose) {
      
      if (!progress) 
        message("Training loss: ", training_loss)
      
    }
    
    # If we have validation data, start the validation
    if (!is.null(validation_data)) {
      
      if (verbose) {
        
        if (!progress)
          message("   Start Validation")
        
      }
      
      # Initialize variables for validation
      loss <- 0
      loss_acc <- 0
      
      if (is.function(validation_data)) {
        
        # If it is a generator
        
        if (verbose) {
          
          if (progress) {
            
            # Progress bar for validation subepoch
            pb_subepoch_val <- progress_bar$new(format = paste0("   Validating Subepoch :subepoch/:total ",
                                                                "[:bar] ETA: :eta . Elapsed: :elapsed. ",
                                                                "Validation loss = :loss"),
                                                total = validation_steps,
                                                clear = FALSE,
                                                width = 100)
            
            pb_subepoch_val$update(ratio = 0, tokens = list(subepoch = 0, loss = Inf))
            
          }
          
        }
        
        # Perform the validation and store the losses
        loss_acc <- rep(0, length(validation_steps))
        for (val_steps in seq(validation_steps)) {
          
          # Use the generator to get data
          test_data <- validation_data()
          
          # Evaluate
          loss <- model %>% evaluate(x = test_data[[1]],
                                     y = test_data[[2]],
                                     batch_size = batch_size,
                                     verbose = 0)
          
          # Store loss (if multi-output, it is a list)
          if (is.list(loss)) loss <- loss$loss
          loss_acc[val_steps] <- loss / num_outputs
          
          if (verbose) {
            
            if (progress)
              pb_subepoch_val$tick(tokens = list(subepoch = val_steps, 
                                                 loss = sprintf("%.5f", loss / num_outputs)))
            
          }
          
        }
        
        # Compute the mean loss
        # loss_acc <- loss_acc / validation_steps
        # loss_acc <- median(loss_acc)
        loss_acc <- mean(loss_acc)
        
      } else {
        
        # If we have validation data, not a generator, just evaluate
        loss_acc <- model %>% evaluate(x = validation_data[[1]],
                                       y = validation_data[[2]])
        
      }
      
      # If we have to keep the best model, and we have reduced the loss, save the model as the new best
      if (keep_best & (loss_acc < best_validation_loss)) {
        
        # Save everything
        best_validation_loss <- loss_acc
        
        saveRDS(object = loss_acc, file = file.path(model_path, model_prefix, paste0(model_prefix, "_loss.rds")))
        
        .model %>% save_model(path = model_path, prefix = model_prefix, 
                              comment = paste0(timestamp(quiet = TRUE), ":", " Model with loss: ", loss_acc, "\n"))
        
        message("New best model found with loss: ", sprintf("%.5f", loss_acc))
        
      }
      
      if (verbose) {
        
        if (!progress)
          message("\nValidation loss: ", loss_acc)
        
      }
      
    }
    
    if (verbose) {
      
      # Update the progress bar
      if (progress) 
        pb_epochs$tick(tokens = list(epoch = epoch))
      
    }
    
  }
  
}
