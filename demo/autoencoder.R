rm(list = ls())
rstudioapi::restartSession()
devtools::load_all("../utils4ni/")
devtools::load_all("../ni.datasets/")

ni.datasets::set_dataset_dir(dir = "/Volumes/Domingo/dlni_data")
devtools::load_all()

##%######################################################%##
#                                                          #
####               Example For Autoencoder              ####
####                   Convolutional                    ####
#                                                          #
##%######################################################%##

require(neurobase)
load_keras()

##%######################################################%##
#                                                          #
####                   Data Loading                     ####
#                                                          #
##%######################################################%##

problem <- "brain_extraction"
problem_path <- problem %>% get_dataset()
info <- problem_path %>% get_problem_info(as_autoencoder = TRUE,
                                          interactive = FALSE)

info %>% split_train_test_sets()

##%######################################################%##
#                                                          #
####                   Network Scheme                   ####
#                                                          #
##%######################################################%##

width <- 7
scheme <- DLscheme$new()

scheme$add(width = width,
           is_autoencoder = TRUE,
           only_convolutionals = FALSE,
           output_width = 3,
           num_features = 3,
           vol_layers_pattern = list(dense(250), dense(100)),
           vol_dropout = 0.15,
           feature_layers = list(dense(10), 
                                 dense(5)),
           feature_dropout = 0.15,
           common_layers = list(dense(250), dense(100)),
           common_dropout = 0.25,
           decoder_layers = list(dense(250), dense(100)),
           last_hidden_layers = list(dense(10)),
           optimizer = "adadelta",
           scale = "meanmax")

scheme$add(memory_limit = "2G")

##%######################################################%##
#                                                          #
####               Network Instantiation                ####
#                                                          #
##%######################################################%##

ae_model <- scheme$instantiate(problem_info = info)

ae_model$summary()

##%######################################################%##
#                                                          #
####                   Model Plotting                   ####
#                                                          #
##%######################################################%##

g <- ae_model$graph()
g %>% plot_graph()
ae_model$plot(to_file = paste0("model_", problem, ".png"))

##%######################################################%##
#                                                          #
####                  Data Generators                   ####
#                                                          #
##%######################################################%##

# By default, 1024 windows are extracted from each file. 
# Use 'use_data' to provide a different number.
target_windows_per_file <- 1024 * 8

ae_model$check_memory()

ae_model$use_data(use = "train",
                  x_files = info$train$x,
                  y_files = info$train$y,
                  target_windows_per_file = target_windows_per_file)

ae_model$use_data(use = "test",
                  x_files = info$test$x,
                  y_files = info$test$y,
                  target_windows_per_file = target_windows_per_file)


##%######################################################%##
#                                                          #
####                        Fit                         ####
#                                                          #
##%######################################################%##

epochs <- 30
keep_best <- TRUE
saving_path <- "/Volumes/Domingo/dlni_models" # Must exist
dir.create(path = saving_path, showWarnings = FALSE, recursive = TRUE)
saving_prefix <- paste0(problem, "_", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"))

ae_model$fit(epochs = epochs,
             keep_best = keep_best,
             path = saving_path,
             prefix = saving_prefix,
             metrics_viewer = TRUE, verbose = TRUE)

ae_model$plot_history()

saving_prefix <- paste0(saving_prefix, "_final")

ae_model$save(path = saving_path, 
              prefix = saving_prefix, 
              comment = "Final model after training")


##%######################################################%##
#                                                          #
####                     Test Image                     ####
#                                                          #
##%######################################################%##

# Select random test image
test_index <- sample(info$test$subject_indices, size = 1)
input_file_list <- lapply(info$inputs, function(x) x[test_index])

# Load images and ground truth
input_imgs <- read_nifti_batch(file_list = input_file_list) 
ground_truth <- input_imgs[[1]]

# ground_truth <- do.call(paste0("scale_", scheme$scale), args = list(ground_truth))

# Infer in the input volume
reconstruction <- ae_model$infer(V = input_imgs)

# Plot Ground Truth results
ortho_plot(x = ground_truth,
           text = "Ground Truth", 
           interactiveness = FALSE)

# Plot Model results
ortho_plot(x = reconstruction * 255, 
           text = "Predicted", 
           interactiveness = FALSE)

rec <- map_images(source = reconstruction,
                  target = ground_truth)

# Plot Model results
ortho_plot(x = rec, 
           text = "Predicted and matched", 
           interactiveness = FALSE)
