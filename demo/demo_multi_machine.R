##%######################################################%##
#                                                          #
####           Example For Brain Multi-Machine          ####
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

problem <- "multi-machine"
info <- problem %>% get_problem_info()

info %>% split_train_test_sets()

##%######################################################%##
#                                                          #
####                   Configuration                    ####
#                                                          #
##%######################################################%##

width <- 7
output_width <- 3
num_features <- 3

vol_layers_pattern <- list(clf(all = TRUE,
                               hidden_layers = list(dense(250),
                                                    dense(200))))

vol_layers <- info %>% create_vol_layers(vol_layers_pattern)

vol_dropout <- 0.25

feature_layers <- list(clf(hidden_layers = list(dense(10), dense(10))))
feature_dropout <- 0.15

common_layers <- list(clf(all = TRUE, hidden_layers = list(dense(250), dense(100))))
# common_layers <- list(dense(250), dense(200))
common_dropout <- 0.25

last_layer_info <- info %>% define_last_layer(units = output_width ^ 3, 
                                              force_categorical = FALSE, 
                                              hidden_layers = list(30, 20))

optimizer <- keras::optimizer_adam()

config <- define_config(window_width = width, 
                        num_features = num_features,
                        vol_layers = vol_layers,
                        vol_dropout = vol_dropout,
                        feature_layers = feature_layers,
                        feature_dropout = feature_dropout,
                        common_layers = common_layers,
                        common_dropout = common_dropout,
                        last_layer_info = last_layer_info,
                        class_balance = FALSE,
                        regularize = TRUE,
                        path = "volumes",
                        optimizer = optimizer, 
                        output_width = output_width,
                        scale = "meanmax",
                        scale_y = "meanmax")

##%######################################################%##
#                                                          #
####                   Model Creation                   ####
#                                                          #
##%######################################################%##


modality_model <- config %>% create_model_from_config()
summary(modality_model$model)

g <- modality_model %>% graph_from_model()
g %>% plot_graph()

##%######################################################%##
#                                                          #
####                  Data Generators                   ####
#                                                          #
##%######################################################%##

max_sub_epochs <- 5

train_config <- config %>% create_generator_from_config(x_files = info$train$x,
                                                        y_files = info$train$y,
                                                        mode = "sampling",
                                                        max_sub_epochs = max_sub_epochs)

test_config <- config %>% create_generator_from_config(x_files = info$test$x,
                                                       y_files = info$test$y,
                                                       mode = "sampling",
                                                       max_sub_epochs = max_sub_epochs)


##%######################################################%##
#                                                          #
####                 Inference Function                 ####
#                                                          #
##%######################################################%##

infer <- config %>% create_inference_function_from_config()

##%######################################################%##
#                                                          #
####                        Fit                         ####
#                                                          #
##%######################################################%##

epochs <- 10
keep_best <- TRUE
saving_path <- file.path(system.file(package = "dl4ni"), "models")
saving_prefix <- paste0(problem, "_", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"))

modality_model %>% fit_with_generator(train_config = train_config, 
                                   validation_config = test_config,
                                   epochs = epochs,
                                   keep_best = keep_best,
                                   path = saving_path,
                                   prefix = saving_prefix)

saving_prefix <- paste0(saving_prefix, "_final")

modality_model %>% save_model(path = saving_path, prefix = saving_prefix, comment = "After training")


##%######################################################%##
#                                                          #
####                     Test Image                     ####
#                                                          #
##%######################################################%##

test_index <- sample(info$test$subject_indices, size = 1)
input_file_list <- lapply(info$inputs, function(x) x[test_index])

input_imgs <- prepare_files_for_inference(file_list = input_file_list) 
ground_truth <- neurobase::readnii(info$outputs[test_index])

modality <- modality_model %>% infer(V = input_imgs, speed = "medium")

ortho_plot(x = input_imgs[[1]], text = "Original image")
ortho_plot(x = ground_truth, text = "Ground Truth")
ortho_plot(x = modality, text = "Predicted")
