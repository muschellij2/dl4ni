##%######################################################%##
#                                                          #
####           Example For Brain Parcellation 2         ####
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

scgm_labels <- c(10, 11, 12, 13, 17, 18, 49:54)
info %>% subset_problem(subset_classes = scgm_labels)

##%######################################################%##
#                                                          #
####                   Configuration                    ####
#                                                          #
##%######################################################%##

width <- 7
output_width <- 3
num_features <- 3

vol_layers_pattern <- list(clf(all = TRUE,
                               hidden_layers = list(dense(300),
                                                    dense(400),
                                                    dense(200),
                                                    dense(100),
                                                    dense(250),
                                                    dense(100))))
vol_layers <- info %>% create_vol_layers(vol_layers_pattern)
vol_dropout <- 0.15

feature_layers <- list(dense(10), dense(5))
feature_dropout <- 0.15

common_layers <- list(clf(all = TRUE, hidden_layers = list(dense(400), 
                                                           dense(200), 
                                                           dense(100))))
common_dropout <- 0.25

last_layer_info <- info %>% define_last_layer(units = output_width ^ 3, 
                                              force_categorical = TRUE,
                                              hidden_layers = list(30, 20))

optimizer <- optimizer_nadam()

config <- define_config(window_width = width, 
                        num_features = num_features,
                        vol_layers = vol_layers,
                        vol_dropout = vol_dropout,
                        feature_layers = feature_layers,
                        feature_dropout = feature_dropout,
                        common_layers = common_layers,
                        common_dropout = common_dropout,
                        last_layer_info = last_layer_info,
                        optimizer = optimizer, 
                        output_width = output_width,
                        scale = "z",
                        scale_y = "none")

##%######################################################%##
#                                                          #
####                   Model Creation                   ####
#                                                          #
##%######################################################%##


parcellation_model <- config %>% create_model_from_config()
summary(parcellation_model$model)

g <- parcellation_model %>% graph_from_model()
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

epochs <- 3
keep_best <- TRUE
saving_path <- file.path(system.file(package = "dl4ni"), "models")
saving_prefix <- paste0(problem, "_", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"))

parcellation_model %>% fit_with_generator(train_config = train_config, 
                                          validation_config = test_config,
                                          epochs = epochs,
                                          keep_best = keep_best,
                                          path = saving_path,
                                          prefix = saving_prefix)

saving_prefix <- paste0(saving_prefix, "_final")

parcellation_model %>% save_model(path = saving_path, prefix = saving_prefix, comment = "After training")


##%######################################################%##
#                                                          #
####                     Test Image                     ####
#                                                          #
##%######################################################%##

test_index <- sample(info$test$subject_indices, size = 1)
input_file_list <- lapply(info$inputs, function(x) x[test_index])

input_imgs <- prepare_files_for_inference(file_list = input_file_list) 
ground_truth <- neurobase::readnii(info$outputs[test_index])
ground_truth <- array(ground_truth * (ground_truth %in% info$remap_classes$source), dim = dim(input_imgs[[1]]))

parcellation <- parcellation_model %>% infer(V = input_imgs, speed = "faster")

num_classes <- length(info$remap_classes$source)
col.y <- scales::alpha(colour = scales::hue_pal()(num_classes), alpha = 0.45)

ortho_plot(x = input_imgs[[1]], y = ground_truth, col.y = col.y, text = "Ground Truth", interactiveness = FALSE)
ortho_plot(x = input_imgs[[1]], y = parcellation, col.y = col.y, text = "Predicted", interactiveness = FALSE)

