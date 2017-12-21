context("DLmodel")

expect_works <- function(object) testthat::expect_error(object, NA)

test_that("DLmodel works as expected for a fully connected model", {
  
  # We'll use a modified BET (non-convolutional) demo
  load_keras()
  
  # Get the dataset
  problem <- "brain_extraction"
  problem_path <- problem %>% get_dataset()
  info <- problem_path %>% get_problem_info(num_subjects = 5, interactive = FALSE)
  
  info %>% split_train_test_sets()
  
  # Model scheme
  scheme <- DLscheme$new()
  
  scheme$add(width = 7,
             only_convolutionals = FALSE,
             output_width = 3,
             num_features = 3,
             vol_layers_pattern = list( 
               dense(25)),
             vol_dropout = 0.15,
             feature_layers = list(dense(10)),
             feature_dropout = 0.15,
             common_layers = list(
               dense(20)),
             common_dropout = 0.25,
             last_hidden_layers = list(dense(10)),
             optimizer = "adadelta",
             scale = "z",
             scale_y = "none")
  
  scheme$add(memory_limit = "1G")
  
  # Network instatiation
  expect_works(bet_model <- scheme$instantiate(problem_info = info))
  expect_works(bet_model <- scheme$instantiate(problem_info = info, prepare_for_training = 2048))
  expect_works(bet_model <- scheme$instantiate(problem_info = info, prepare_for_training = FALSE))
  
  expect_works(bet_model$plot(to_file = tempfile(fileext = ".png")))
  
  expect_is(bet_model, "DLmodel")
  
  # By default, 1024 windows are extracted from each file. 
  # Use 'use_data' to provide a different number.
  target_windows_per_file <- 1024
  
  expect_works(bet_model$check_memory())
  
  expect_works(bet_model$use_data(use = "train",
                                  x_files = info$train$x,
                                  y_files = info$train$y,
                                  target_windows_per_file = target_windows_per_file))
  
  expect_works(bet_model$use_data(use = "test",
                                  x_files = info$test$x,
                                  y_files = info$test$y,
                                  target_windows_per_file = target_windows_per_file))
  
  # Training
  epochs <- 1
  keep_best <- TRUE
  saving_path <- file.path(system.file(package = "dl4ni"), "models")
  saving_prefix <- paste0(problem, "_", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"))
  
  expect_works(bet_model$fit(epochs = epochs,
                             keep_best = keep_best,
                             path = saving_path,
                             prefix = saving_prefix,
                             metrics_viewer = FALSE))
  
  saving_prefix <- paste0(saving_prefix, "_final")
  
  expect_works(bet_model$save(path = saving_path, 
                              prefix = saving_prefix, 
                              comment = "Final model after training"))
  
  # Select random test image
  test_index <- sample(info$test$subject_indices, size = 1)
  input_file_list <- lapply(info$inputs, function(x) x[test_index])
  
  # Load images and ground truth
  input_imgs <- prepare_files_for_inference(file_list = input_file_list) 
  ground_truth <- read_nifti_to_array(info$outputs[test_index])
  
  # Infer in the input volume
  expect_works(brain <- bet_model$infer(V = input_imgs, speed = "faster", verbose = FALSE))
  expect_works(ortho_plot(input_imgs[1]))
  expect_works(ortho_plot(input_imgs[1], brain))
  
  # The model can be resetted
  expect_works(bet_model$reset())
  
})
