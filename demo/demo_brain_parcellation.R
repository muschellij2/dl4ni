
##%######################################################%##
#                                                          #
####           Example For Brain Parcellation           ####
#                                                          #
##%######################################################%##


####                    Data Loading                    ####


setwd("~/Downloads/deep_learning")

base_dir <- "/Users/domingolopez/Downloads/diff_patch_volumetry_multiatlas/npbd.atlases/inst/subcortical/"
all_images <- list.files(path = base_dir, pattern = ".nii.gz", full.names = TRUE)
mask_images <- list.files(path = base_dir, pattern = "_DKT31_CMA_labels_in_MNI152.nii.gz", full.names = TRUE)

templates <- setdiff(all_images, mask_images)

subject_index <- seq_along(templates)
train_size <- round(0.75 * length(subject_index))
train_indices <- sample(subject_index, size = train_size)
test_indices <- setdiff(subject_index, train_indices)

scgm_labels <- c(10, 11, 12, 13, 17, 18, 49:54)

####                       KERAS                        ####

Sys.setenv(KERAS_BACKEND = "tensorflow")
library(keras)


width <- 7

# vol_layers <- c(500, 200, 100, 200, width ^ 3)
# common_layers <- c(1000, 500, 250, 100, 250)
vol_layers <- c(200, 100, 100, width ^ 3)
common_layers <- c(500, 250, 100, 250)

optimizer <- optimizer_adam()
loss <- loss_mean_squared_error
# loss <- dice_loss

new_model <- create_and_compile_model(width, 
                                      vol_layers = vol_layers, 
                                      initialize_with_lstm = FALSE,
                                      common_layers = common_layers, 
                                      finalize_with_convolutional = FALSE,
                                      clf = TRUE, 
                                      resnet = FALSE,
                                      optimizer = optimizer, 
                                      loss = loss)
summary(new_model$model)

# train and evaluate
# Data generators
num_windows <- 15000
max_sub_epochs <- round(60000 / num_windows)
label_to_model <- scgm_labels
scale_type <- "z"

train_generator_array <- nifti_files_generator_sampling(x_files = templates[train_indices], 
                                                        y_files = mask_images[train_indices],
                                                        y_label = label_to_model, 
                                                        minimum_sample_weight = NULL,
                                                        class_balance = TRUE,
                                                        scale = scale_type,
                                                        width = width, 
                                                        num_windows = num_windows, 
                                                        max_sub_epochs = max_sub_epochs)

test_generator_array <- nifti_files_generator_sampling(x_files = templates[test_indices], 
                                                        y_files = mask_images[test_indices],
                                                        y_label = label_to_model, 
                                                        minimum_sample_weight = NULL,
                                                        class_balance = TRUE,
                                                        scale = scale_type,
                                                        width = width, 
                                                        num_windows = num_windows, 
                                                        max_sub_epochs = max_sub_epochs)


# test_generator_array <- nifti_files_generator_all(x_files = templates[test_indices], 
#                                                   y_files = mask_images[test_indices],
#                                                   y_label = label_to_model,
#                                                   scale = scale_type,
#                                                   width = width)

new_model %>% fit_with_generator(generator = train_generator_array,
                                 steps_per_epoch = length(train_indices) * max_sub_epochs, 
                                 validation_data = test_generator_array,
                                 validation_steps = length(test_indices) * max_sub_epochs,
                                 epochs = 20,
                                 batch_size = min(5000, num_windows),
                                 keep_best = TRUE)

####                   Inference                        ####

new_index <- sample(test_indices, size = 1)

new_img <- neurobase::readnii(templates[new_index])
ground_truth <- neurobase::readnii(mask_images[new_index])
ground_truth <- array(ground_truth %in% label_to_model, dim = dim(new_img))

new_seg <- new_model %>% infer_volume(V = as.array(new_img), 
                                      scale = scale_type,
                                      regularize = FALSE, 
                                      max_batch_size = num_windows,
                                      fast = TRUE)

new_seg_slow <- new_model %>% infer_volume(V = as.array(new_img), 
                                      scale = scale_type,
                                      regularize = FALSE, 
                                      max_batch_size = num_windows,
                                      fast = FALSE)
gc()

####                   Visualization                    ####

require(neurobase)
# red05 <- scales::alpha("red", 0.5)
# green05 <- scales::alpha("green", alpha = 0.5)
# blue05 <- scales::alpha("blue", alpha = 0.5)

ortho2(new_img, ground_truth, col.y = red05, text = "Correct")
ortho2(new_img, new_seg, col.y = red05, text = "Predicted Fast")
ortho2(new_img, new_seg_slow, col.y = red05, text = "Predicted Slow")

ortho_diff(new_img, 
           pred = niftiarr(arr = new_seg, new_img), 
           roi = niftiarr(arr = ground_truth, new_img), 
           addlegend = TRUE)

gc()
