##%######################################################%##
#                                                          #
####           Example For Brain Transformation         ####
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

problem <- "t1_flair"
info <- problem %>% get_problem_info()

info %>% split_train_test_sets()

##%######################################################%##
#                                                          #
####                   Network Scheme                   ####
#                                                          #
##%######################################################%##

width <- 32

scheme <- create_scheme(width = width,
                        only_convolutionals = TRUE,
                        output_width = width,
                        num_features = 3,
                        vol_layers_pattern = segnet(depth = as.integer(log2(width) - 1), 
                                                    mode = "convolutional", 
                                                    initial_filters = 4),
                        vol_dropout = 0,
                        feature_layers = list(),
                        feature_dropout = 0,
                        common_layers = list(),
                        common_dropout = 0,
                        last_hidden_layers = list(),
                        optimizer = "nadam",
                        scale = "z",
                        scale_y = "none")

##%######################################################%##
#                                                          #
####               Network Instantiation                ####
#                                                          #
##%######################################################%##

flair_model <- scheme %>% instantiate_model(problem_info = info)

summary(flair_model$model)

##%######################################################%##
#                                                          #
####                   Model Plotting                   ####
#                                                          #
##%######################################################%##

g <- flair_model %>% graph_from_model()
g %>% plot_graph()
flair_model %>% plot_model(to_file = paste0("model_", problem, ".png"))

##%######################################################%##
#                                                          #
####                  Data Generators                   ####
#                                                          #
##%######################################################%##

target_windows_per_file <- 1000

batch_size <- bet_model %>% compute_batch_size()

if (batch_size == 0) {
  
  message("Do not continue!! Not enough memory!!")
  
}

batches_per_file <- as.integer(target_windows_per_file / batch_size)

train_config <- flair_model %>% create_generator(x_files = info$train$x,
                                                 y_files = info$train$y,
                                                 batches_per_file = batches_per_file)

test_config <- flair_model %>% create_generator(x_files = info$test$x,
                                                y_files = info$test$y,
                                                batches_per_file = batches_per_file)


##%######################################################%##
#                                                          #
####                        Fit                         ####
#                                                          #
##%######################################################%##

epochs <- 15
keep_best <- TRUE
saving_path <- file.path(system.file(package = "dl4ni"), "models")
saving_prefix <- paste0(problem, "_", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"))

flair_model %>% fit_with_generator(train_config = train_config, 
                                   validation_config = test_config,
                                   epochs = epochs,
                                   starting_epoch = 1,
                                   keep_best = keep_best,
                                   path = saving_path,
                                   prefix = saving_prefix)

saving_prefix <- paste0(saving_prefix, "_final")

flair_model %>% save_model(path = saving_path, 
                           prefix = saving_prefix, 
                           comment = "Final model after training")

# flair_model <- load_model(path = saving_path, prefix = saving_prefix)


##%######################################################%##
#                                                          #
####                     Test Image                     ####
#                                                          #
##%######################################################%##

# Select random test subject
test_index <- sample(info$test$subject_indices, size = 1)
input_file_list <- lapply(info$inputs, function(x) x[test_index])

# Read images and ground truth
input_imgs <- prepare_files_for_inference(file_list = input_file_list) 
ground_truth <- neurobase::readnii(info$outputs[test_index])

# Predict on the inputs
flair <- flair_model %>% infer(V = input_imgs, speed = "faster")

# Plot
ortho_plot(x = input_imgs[[1]], text = "Original image", interactiveness = FALSE)
ortho_plot(x = ground_truth, text = "Ground Truth", interactiveness = FALSE)
ortho_plot(x = flair, text = "Predicted", interactiveness = FALSE)
