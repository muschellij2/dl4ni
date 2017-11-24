rm(list = ls())
rstudioapi::restartSession()
devtools::load_all()
devtools::load_all("../dl4ni.data/")

##%######################################################%##
#                                                          #
####             Example For Brain Parcellation         ####
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

problem <- "parcellation"
info <- problem %>% get_problem_info()

info %>% split_train_test_sets()

cortex <- c(6, 45, 630:3000)
scgm_labels <- c(10, 11, 12, 13, 17, 18, 49:54)
spinal_cord_labels <- 16
ventricles_labels <- c(4, 5, 14, 15, 24, 43, 44, 72)
info %>% subset_problem(subset_classes = scgm_labels, 
                        unify_classes = list(cortex, 
                                             spinal_cord_labels, 
                                             ventricles_labels))

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
                                                              dense(400),
                                                              dense(200),
                                                              dense(100),
                                                              dense(250),
                                                              dense(100)))),
           vol_dropout = 0.15,
           feature_layers = list(dense(10), 
                                 dense(5)),
           feature_dropout = 0.15,
           common_layers = list(clf(all = TRUE, 
                                    hidden_layers = list(dense(400), 
                                                         dense(200), 
                                                         dense(100)))),
           common_dropout = 0.25,
           last_hidden_layers = list(30, 20),
           optimizer = "nadam",
           scale = "z",
           scale_y = "none")

scheme$add(memory_limit = "2G")

##%######################################################%##
#                                                          #
####               Network Instantiation                ####
#                                                          #
##%######################################################%##

parcellation_model <- scheme$instantiate(problem_info = info)

parcellation_model$summary()

##%######################################################%##
#                                                          #
####                   Model Plotting                   ####
#                                                          #
##%######################################################%##

g <- parcellation_model$graph()
g %>% plot_graph()
parcellation_model$plot(to_file = paste0("model_", problem, ".png"))

##%######################################################%##
#                                                          #
####                  Data Generators                   ####
#                                                          #
##%######################################################%##

# By default, 1024 windows are extracted from each file. 
# Use 'use_data' to provide a different number.
target_windows_per_file <- 1024

parcellation_model$check_memory()

parcellation_model$use_data(use = "train",
                            x_files = info$train$x,
                            y_files = info$train$y,
                            target_windows_per_file = target_windows_per_file)

parcellation_model$use_data(use = "test",
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
saving_path <- file.path(system.file(package = "dl4ni"), "models")
saving_prefix <- paste0(problem, "_", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"))

parcellation_model$fit(epochs = epochs,
                       keep_best = keep_best,
                       path = saving_path,
                       prefix = saving_prefix)

parcellation_model$plot_history()

saving_prefix <- paste0(saving_prefix, "_final")

parcellation_model$save(path = saving_path, 
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
ground_truth <- read_nifti_to_array(info$outputs[test_index])
ground_truth <- map_ids(ground_truth, remap_classes = info$remap_classes)

# Infer in the input volume
parcellation <- parcellation_model$infer(V = input_imgs, speed = "faster")
parcellation <- map_ids(parcellation, remap_classes = info$remap_classes)

# Some values for plotting
num_classes <- length(info$remap_classes$target)
col.y <- scales::alpha(colour = scales::hue_pal()(num_classes), alpha = 0.45)

# Plot Ground Truth results
ortho_plot(x = input_imgs[[1]], 
           y = ground_truth, 
           col.y = col.y, 
           text = "Ground Truth", 
           interactiveness = FALSE)

# Plot Model results
ortho_plot(x = input_imgs[[1]], 
           y = parcellation, 
           col.y = col.y, 
           text = "Predicted", 
           interactiveness = FALSE)

