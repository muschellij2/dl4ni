##%######################################################%##
#                                                          #
####        Example For Brain Tumor Segmentation        ####
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

problem <- "brats_lgg"
info <- problem %>% get_problem_info(num_subjects = 10)

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

##%######################################################%##
#                                                          #
####               Network Instantiation                ####
#                                                          #
##%######################################################%##

tumor_model <- scheme %>% instantiate_model(problem_info = info)

summary(tumor_model$model)

##%######################################################%##
#                                                          #
####                   Model Plotting                   ####
#                                                          #
##%######################################################%##

g <- tumor_model %>% graph_from_model()
g %>% plot_graph()
tumor_model %>% plot_model(to_file = paste0("model_", problem, ".png"))

##%######################################################%##
#                                                          #
####                  Data Generators                   ####
#                                                          #
##%######################################################%##

target_windows_per_file <- 1000

batch_size <- tumor_model %>% compute_batch_size()

if (batch_size == 0) {
  
  message("Do not continue!! Not enough memory!!")
  
}

batches_per_file <- as.integer(target_windows_per_file / batch_size)

train_config <- tumor_model %>% create_generator(x_files = info$train$x,
                                               y_files = info$train$y,
                                               batches_per_file = batches_per_file)

test_config <- tumor_model %>% create_generator(x_files = info$test$x,
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

tumor_model %>% fit_with_generator(train_config = train_config, 
                                 validation_config = test_config,
                                 epochs = epochs,
                                 starting_epoch = 1,
                                 keep_best = keep_best,
                                 path = saving_path,
                                 prefix = saving_prefix)

saving_prefix <- paste0(saving_prefix, "_final")

tumor_model %>% save_model(path = saving_path, 
                         prefix = saving_prefix, 
                         comment = "Final model after training")

# tumor_model <- load_model(path = saving_path, prefix = saving_prefix)

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
tumor <- tumor_model %>% infer_on_volume(V = input_imgs, speed = "faster")

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
