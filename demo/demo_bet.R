rm(list = ls())
rstudioapi::restartSession()
devtools::load_all()
devtools::load_all("../dl4ni.data/")

##%######################################################%##
#                                                          #
####            Example For Brain Extraction            ####
#                                                          #
##%######################################################%##

require(neurobase)
require(dl4ni.data)
load_keras()
K <- keras::backend()
tensorflow::tf$set_random_seed(1234)
set.seed(1234)

##%######################################################%##
#                                                          #
####                   Data Loading                     ####
#                                                          #
##%######################################################%##

problem <- "brain_extraction"
info <- problem %>% get_problem_info()

info %>% split_train_test_sets()

##%######################################################%##
#                                                          #
####                   Network Scheme                   ####
#                                                          #
##%######################################################%##

scheme <- create_scheme(width = 7,
                        only_convolutionals = FALSE,
                        output_width = 3,
                        num_features = 3,
                        vol_layers_pattern = list(clf(all = TRUE, 
                                                      hidden_layers = list( 
                                                        dense(250),
                                                        dense(100)))),
                        vol_dropout = 0.15,
                        feature_layers = list(dense(10), 
                                              dense(5)),
                        feature_dropout = 0.15,
                        common_layers = list(clf(all = TRUE, 
                                                 hidden_layers = list( 
                                                   dense(200),
                                                   dense(100)))),
                        common_dropout = 0.25,
                        last_hidden_layers = list(dense(10)),
                        optimizer = "adadelta",
                        scale = "z",
                        scale_y = "none")

scheme %>% add_attribute(memory_limit = "3G")

##%######################################################%##
#                                                          #
####               Network Instantiation                ####
#                                                          #
##%######################################################%##

bet_model <- scheme %>% instantiate_model(problem_info = info)

bet_model$summary()

##%######################################################%##
#                                                          #
####                   Model Plotting                   ####
#                                                          #
##%######################################################%##

g <- bet_model %>% graph_from_model()
g %>% plot_graph()
bet_model %>% plot_model(to_file = paste0("model_", problem, ".png"))

##%######################################################%##
#                                                          #
####                  Data Generators                   ####
#                                                          #
##%######################################################%##


target_windows_per_file <- 1024

bet_model$check_memory()

train_config <- bet_model %>% create_generator(x_files = info$train$x,
                                               y_files = info$train$y,
                                               target_windows_per_file = target_windows_per_file)

test_config <- bet_model %>% create_generator(x_files = info$test$x,
                                              y_files = info$test$y,
                                              target_windows_per_file = target_windows_per_file)


##%######################################################%##
#                                                          #
####                        Fit                         ####
#                                                          #
##%######################################################%##

epochs <- 5
keep_best <- TRUE
saving_path <- file.path(system.file(package = "dl4ni"), "models")
saving_prefix <- paste0(problem, "_", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"))

bet_model %>% fit_with_generator(train_config = train_config, 
                                 validation_config = test_config,
                                 epochs = epochs,
                                 starting_epoch = 1,
                                 keep_best = keep_best,
                                 path = saving_path,
                                 prefix = saving_prefix,
                                 metrics_viewer = FALSE,
                                 reset_optimizer = FALSE)

bet_model$plot_history()

saving_prefix <- paste0(saving_prefix, "_final")

bet_model %>% save_model(path = saving_path, 
                         prefix = saving_prefix, 
                         comment = "Final model after training")

# bet_model <- load_model(path = saving_path, prefix = saving_prefix)

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
ground_truth <- neurobase::readnii(info$outputs[test_index])

# Infer in the input volume
brain <- bet_model %>% infer_on_volume(V = input_imgs, speed = "faster")

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
           y = brain > 0, 
           col.y = col.y, 
           text = "Predicted", 
           interactiveness = FALSE)

