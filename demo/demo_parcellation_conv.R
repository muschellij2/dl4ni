rm(list = ls())
rstudioapi::restartSession()
devtools::load_all()
devtools::load_all("../dl4ni.data/")

##%######################################################%##
#                                                          #
####             Example For Brain Parcellation         ####
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

parcellation_model <- scheme %>% instantiate_model(problem_info = info)

summary(parcellation_model$model)

##%######################################################%##
#                                                          #
####                   Model Plotting                   ####
#                                                          #
##%######################################################%##

g <- parcellation_model %>% graph_from_model()
g %>% plot_graph()
parcellation_model %>% plot_model(to_file = paste0("model_", problem, ".png"))

##%######################################################%##
#                                                          #
####                  Data Generators                   ####
#                                                          #
##%######################################################%##

target_windows_per_file <- 1000

batch_size <- parcellation_model %>% compute_batch_size()

if (batch_size == 0) {
  
  message("Do not continue!! Not enough memory!!")
  
}

batches_per_file <- as.integer(target_windows_per_file / batch_size)

train_config <- parcellation_model %>% create_generator(x_files = info$train$x,
                                                        y_files = info$train$y,
                                                        batches_per_file = batches_per_file)

test_config <- parcellation_model %>% create_generator(x_files = info$test$x,
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

parcellation_model %>% fit_with_generator(train_config = train_config, 
                                          validation_config = test_config,
                                          epochs = epochs,
                                          starting_epoch = 1,
                                          keep_best = keep_best,
                                          path = saving_path,
                                          prefix = saving_prefix)

saving_prefix <- paste0(saving_prefix, "_final")

parcellation_model %>% save_model(path = saving_path, 
                                  prefix = saving_prefix, 
                                  comment = "Final model after training")

# parcellation_model <- load_model(path = saving_path, prefix = saving_prefix)

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
ground_truth <- map_ids(ground_truth, remap_classes = info$remap_classes)

# Infer in the input volume
parcellation <- parcellation_model %>% infer_on_volume(V = input_imgs, speed = "faster")
parcellation <- map_ids(parecellation, remap_classes = info$remap_classes)

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

