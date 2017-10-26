##%######################################################%##
#                                                          #
####           Example For Brain Extraction             ####
#                                                          #
##%######################################################%##


####                    Data Loading                    ####

setwd("~/Downloads/deep_learning")
gc()

base_dir <- "/Users/domingolopez/Downloads/diff_patch_volumetry_multiatlas/npbd.atlases/inst/brain_extraction"
all_images <- list.files(path = base_dir, pattern = ".nii", full.names = TRUE)
mask_images <- list.files(path = base_dir, pattern = "_brainmask.nii", full.names = TRUE)

templates <- setdiff(all_images, mask_images)

subject_index <- seq_along(templates)
train_size <- round(0.75 * length(subject_index))
train_indices <- sample(subject_index, size = train_size)
test_indices <- setdiff(subject_index, train_indices)

####                       KERAS                        ####

Sys.setenv(KERAS_BACKEND = "tensorflow")
library(keras)

width <- 7

vol_layers <- c(500, 200, 100, 200, width ^ 3)
common_layers <- c(1000, 500, 250, 100, 250)

# vol_layers <- c(200, 100, width ^ 3)
# common_layers <- c(250, 100, 250)

optimizer <- optimizer_adam()
loss <- loss_mean_squared_error

model <- create_and_compile_model(width, 
                                  vol_layers, 
                                  common_layers, 
                                  clf = TRUE, 
                                  optimizer = optimizer, 
                                  loss = loss)

summary(model$model)

# train and evaluate
# Data generators
max_sub_epochs <- 5
label_to_model <- 1
scale_type <- "z"

train_generator_array <- nifti_files_generator_sampling(x_files = templates[train_indices], 
                                                        y_files = mask_images[train_indices],
                                                        y_label = label_to_model, 
                                                        minimum_sample_weight = NULL,
                                                        class_balance = FALSE,
                                                        scale = scale_type,
                                                        width = width, 
                                                        num_windows = 10000, 
                                                        max_sub_epochs = max_sub_epochs)

test_generator_array <- nifti_files_generator_all(x_files = templates[test_indices], 
                                                  y_files = mask_images[test_indices],
                                                  y_label = label_to_model,
                                                  scale = scale_type,
                                                  width = width)

model %>% fit_with_generator(generator = train_generator_array,
                             steps_per_epoch = length(train_indices) * max_sub_epochs, 
                             validation_data = test_generator_array,
                             validation_steps = length(test_indices),
                             keep_best = TRUE,
                             epochs = 5,
                             batch_size = 2000)


####                   Inference                        ####

new_index <- sample(test_indices, size = 1)

new_img <- neurobase::readnii(templates[new_index])
ground_truth <- neurobase::readnii(mask_images[new_index])
ground_truth <- ground_truth == label_to_model

new_brain <- model %>% infer_volume(V = as.array(new_img), scale = scale_type)
gc()

####                   Visualization                    ####

require(neurobase)
ortho_diff(new_img, 
           pred = niftiarr(arr = new_brain, new_img), 
           roi = niftiarr(arr = ground_truth, new_img), 
           addlegend = TRUE)

gc()

