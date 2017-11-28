rm(list = ls())
rstudioapi::restartSession()
devtools::load_all()
devtools::load_all("../dl4ni.data/")

##%######################################################%##
#                                                          #
####               Example For Autoencoder              ####
####                   Convolutional                    ####
#                                                          #
##%######################################################%##

require(neurobase)
require(dl4ni.data)
load_keras()

##%######################################################%##
#                                                          #
####                   Data Loading                     ####
#                                                          #
##%######################################################%##

problem <- "brain_extraction"
problem_path <- problem %>% get_dataset()
info <- problem_path %>% get_problem_info()

info %>% split_train_test_sets()

##%######################################################%##
#                                                          #
####                   Network Scheme                   ####
#                                                          #
##%######################################################%##

width <- 32
scheme <- DLscheme$new()

scheme$add(width = width,
           is_autoencoder = TRUE,
           loss = keras::loss_mean_squared_error,
           only_convolutionals = TRUE,
           output_width = width,
           num_features = 3,
           vol_layers_pattern = segnet(depth = as.integer(log2(width) - 1), 
                                       mode = "convolutional", 
                                       initial_filters = 2),
           vol_dropout = 0,
           feature_layers = list(),
           feature_dropout = 0,
           common_layers = list(conv3d(filters = 1, kernel_size = c(1, 1, 1))),
           common_dropout = 0,
           decoder_layers = c(segnet(depth = as.integer(log2(width) - 1), 
                                     mode = "convolutional", 
                                     initial_filters = 2), 
                              list(conv3d(filters = 1, 
                                          kernel_size = c(1, 1, 1)))),
           add_last_layer = FALSE,
           last_hidden_layers = list(),
           optimizer = "adadelta",
           scale = "meanmax",
           scale_y = "none")

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
target_windows_per_file <- 1024

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

epochs <- 1
keep_best <- TRUE
saving_path <- file.path(system.file(package = "dl4ni"), "models")
saving_prefix <- paste0(problem, "_", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"))

ae_model$fit(epochs = epochs,
             keep_best = keep_best,
             path = saving_path,
             prefix = saving_prefix)

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
input_imgs <- prepare_files_for_inference(file_list = input_file_list) 
ground_truth <- input_imgs[[1]]

# Infer in the input volume
reconstruction <- ae_model$infer(V = input_imgs, speed = "faster")

# Plot Ground Truth results
ortho_plot(x = ground_truth,
           text = "Ground Truth", 
           interactiveness = FALSE)

# Plot Model results
ortho_plot(x = reconstruction, 
           text = "Predicted", 
           interactiveness = FALSE)

