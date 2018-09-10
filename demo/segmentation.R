rm(list = ls())
rstudioapi::restartSession()
devtools::load_all("../utils4ni/")
devtools::load_all("../ni.datasets/")

ni.datasets::set_dataset_dir(dir = "/Volumes/Domingo/dlni_data")
devtools::load_all()

##%######################################################%##
#                                                          #
####            Example For Brain Segmentation          ####
#                                                          #
##%######################################################%##

require(neurobase)
load_keras()

##%######################################################%##
#                                                          #
####                   Data Loading                     ####
#                                                          #
##%######################################################%##

problem <- "segmentation"
problem_path <- problem %>% get_dataset()
info <- problem_path %>% get_problem_info()

info %>% split_train_test_sets()

##%######################################################%##
#                                                          #
####                   Network Scheme                   ####
#                                                          #
##%######################################################%##

scheme <- DLscheme$new()

scheme$add(width = 7,
           only_convolutionals = FALSE,
           output_width = 3,
           num_features = 3,
           vol_layers_pattern = list(clf(all = TRUE,
                                         hidden_layers = list(dense(200),
                                                              dense(100),
                                                              dense(250),
                                                              dense(100)))),
           vol_dropout = 0.15,
           feature_layers = list(dense(10)),
           feature_dropout = 0.15,
           common_layers = list(clf(all = TRUE, 
                                    hidden_layers = list(dense(100)))),
           common_dropout = 0.25,
           last_hidden_layers = list(15, 10),
           optimizer = "nadam",
           scale = "z",
           scale_y = "none")

##%######################################################%##
#                                                          #
####               Network Instantiation                ####
#                                                          #
##%######################################################%##

segmentation_model <- scheme$instantiate(problem_info = info)

segmentation_model$summary()

##%######################################################%##
#                                                          #
####                   Model Plotting                   ####
#                                                          #
##%######################################################%##

g <- segmentation_model$graph()
g %>% plot_graph()
segmentation_model$plot(to_file = paste0("model_", problem, ".png"))

##%######################################################%##
#                                                          #
####                  Data Generators                   ####
#                                                          #
##%######################################################%##

# By default, 1024 windows are extracted from each file. 
# Use 'use_data' to provide a different number.
target_windows_per_file <- 1024

segmentation_model$check_memory()

segmentation_model$use_data(use = "train",
                            x_files = info$train$x,
                            y_files = info$train$y,
                            target_windows_per_file = target_windows_per_file)

segmentation_model$use_data(use = "test",
                            x_files = info$test$x,
                            y_files = info$test$y,
                            target_windows_per_file = target_windows_per_file)


##%######################################################%##
#                                                          #
####                        Fit                         ####
#                                                          #
##%######################################################%##

epochs <- 15
keep_best <- TRUE
saving_path <- "/Volumes/Domingo/dlni_models" # Must exist
dir.create(path = saving_path, showWarnings = FALSE, recursive = TRUE)
saving_prefix <- paste0(problem, "_", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"))

segmentation_model$fit(epochs = epochs,
                       keep_best = keep_best,
                       path = saving_path,
                       prefix = saving_prefix)

segmentation_model$plot_history()

saving_prefix <- paste0(saving_prefix, "_final")

segmentation_model$save(path = saving_path, 
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
ground_truth <- read_nifti_to_array(info$outputs[test_index])

# Infer in the input volume
segmentation <- segmentation_model$infer(V = input_imgs, speed = "faster")

# Some values for plotting
num_classes <- length(info$values)
col.y <- scales::alpha(colour = scales::viridis_pal()(num_classes), alpha = 0.25)

# Plot Ground Truth results
ortho_plot(x = input_imgs[[1]], 
           y = ground_truth, 
           col.y = col.y, 
           text = "Ground Truth", 
           interactiveness = FALSE)

# Plot Model results
ortho_plot(x = input_imgs[[1]], 
           y = segmentation, 
           col.y = col.y, 
           text = "Predicted", 
           interactiveness = FALSE)

"segmentation2_2017_10_31_13_23_02_final"
