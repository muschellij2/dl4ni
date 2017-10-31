rm(list = ls())
rstudioapi::restartSession()
devtools::load_all()
devtools::load_all("../dl4ni.data/")

##%######################################################%##
#                                                          #
####           Example For Brain Segmentation 2         ####
#                                                          #
##%######################################################%##

require(neurobase)
require(dl4ni.data)

##%######################################################%##
#                                                          #
####                   Data Loading                     ####
#                                                          #
##%######################################################%##

problem <- "segmentation2"
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
                               hidden_layers = list(dense(200),
                                                    dense(100))))
vol_layers <- info %>% create_vol_layers(vol_layers_pattern)
vol_dropout <- 0.15

feature_layers <- list(dense(10), dense(5))
feature_dropout <- 0.15

# common_layers <- list(dense(1000), dense(500), dense(250), dense(100), dense(250))
common_layers <- list(dense(500), dense(250), dense(100))
common_dropout <- 0.25

last_layer_info <- info %>% define_last_layer(units = output_width ^ 3, 
                                              force_categorical = FALSE, 
                                              multioutput = TRUE,
                                              hidden_layers = c(10))


optimizer <- keras::optimizer_nadam()

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
                        optimizer = optimizer, 
                        output_width = output_width,
                        scale = "z",
                        scale_y = "none")

##%######################################################%##
#                                                          #
####                   Model Creation                   ####
#                                                          #
##%######################################################%##


segmentation_model <- config %>% create_model_from_config()
summary(segmentation_model$model)

g <- segmentation_model %>% graph_from_model()
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

epochs <- 30
keep_best <- TRUE
saving_path <- file.path(system.file(package = "dl4ni"), "models")
saving_prefix <- paste0(problem, "_", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"))

segmentation_model %>% fit_with_generator(train_config = train_config, 
                                          validation_config = test_config,
                                          epochs = epochs,
                                          starting_epoch = 1,
                                          keep_best = keep_best,
                                          path = saving_path,
                                          prefix = saving_prefix)

saving_prefix <- paste0(saving_prefix, "_final")

segmentation_model %>% save_model(path = saving_path, prefix = saving_prefix, comment = "After training")


##%######################################################%##
#                                                          #
####                     Test Image                     ####
#                                                          #
##%######################################################%##

test_index <- sample(info$test$subject_indices, size = 1)
input_file_list <- lapply(info$inputs, function(x) x[test_index])

input_imgs <- prepare_files_for_inference(file_list = input_file_list) 
ground_truth <- neurobase::readnii(info$outputs[test_index])

segmentation <- segmentation_model %>% infer(V = input_imgs, speed = "faster")

num_classes <- length(info$values)
col.y <- scales::alpha(colour = scales::viridis_pal()(num_classes), alpha = 0.25)

ortho_plot(x = input_imgs[[1]], y = ground_truth, col.y = col.y, text = "Ground Truth", interactiveness = FALSE)
ortho_plot(x = input_imgs[[1]], y = segmentation, col.y = col.y, text = "Predicted", interactiveness = FALSE)
