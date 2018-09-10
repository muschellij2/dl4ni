rm(list = ls())
rstudioapi::restartSession()
devtools::load_all("../utils4ni/")
devtools::load_all("../ni.datasets/")

ni.datasets::set_dataset_dir(dir = "/Volumes/Domingo/dlni_data")
devtools::load_all()

##%######################################################%##
#                                                          #
####        Example For Brain Tumor Segmentation        ####
#                                                          #
##%######################################################%##

require(neurobase)
load_keras()

##%######################################################%##
#                                                          #
####                   Data Loading                     ####
#                                                          #
##%######################################################%##

problem <- "brats_lgg"
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
                                         hidden_layers = list(dense(300),
                                                              dense(200),
                                                              dense(100),
                                                              dense(250),
                                                              dense(100)))),
           vol_dropout = 0.15,
           feature_layers = list(dense(10), 
                                 dense(5)),
           feature_dropout = 0.15,
           common_layers = list(clf(all = TRUE, 
                                    hidden_layers = list(dense(300), 
                                                         dense(200), 
                                                         dense(100)))),
           common_dropout = 0.25,
           last_hidden_layers = list(dense(10)),
           optimizer = "adadelta",
           scale = "z",
           scale_y = "none")

scheme$add(memory_limit = "2G")


##%######################################################%##
#                                                          #
####               Network Instantiation                ####
#                                                          #
##%######################################################%##

tumor_model <- scheme$instantiate(problem_info = info)

tumor_model$summary()

##%######################################################%##
#                                                          #
####                   Model Plotting                   ####
#                                                          #
##%######################################################%##

g <- tumor_model$graph()
g %>% plot_graph()
tumor_model$plot(to_file = paste0("model_", problem, ".png"))

##%######################################################%##
#                                                          #
####                  Data Generators                   ####
#                                                          #
##%######################################################%##

# By default, 1024 windows are extracted from each file. 
# Use 'use_data' to provide a different number.
target_windows_per_file <- 1024

tumor_model$check_memory()

tumor_model$use_data(use = "train",
                     x_files = info$train$x,
                     y_files = info$train$y,
                     target_windows_per_file = target_windows_per_file)

tumor_model$use_data(use = "test",
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

tumor_model$fit(epochs = epochs,
                keep_best = keep_best,
                path = saving_path,
                prefix = saving_prefix)

tumor_model$plot_history()

saving_prefix <- paste0(saving_prefix, "_final")

tumor_model$save(path = saving_path, 
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
tumor <- tumor_model$infer(V = input_imgs, speed = "faster")

# Some values for plotting
num_classes <- length(info$values)
col.y <- scales::alpha(colour = scales::hue_pal()(num_classes), alpha = 0.45)

# Plot Ground Truth results
ortho_plot(x = input_imgs[[1]], 
           y = ground_truth, 
           col.y = col.y, 
           text = "Ground Truth", 
           interactiveness = FALSE)

# Plot Model results
ortho_plot(x = input_imgs[[1]], 
           y = tumor, 
           col.y = col.y, 
           text = "Predicted", 
           interactiveness = FALSE)
