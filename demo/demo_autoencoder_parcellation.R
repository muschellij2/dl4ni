
##%######################################################%##
#                                                          #
####           Example For Brain Parcellation AE        ####
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
num_classes <- length(scgm_labels)

####                       KERAS                        ####

Sys.setenv(KERAS_BACKEND = "tensorflow")
library(keras)

width <- 7

# vol_layers <- c(500, 200, 100, 200, width ^ 3)
# common_layers <- c(1000, 500, 250, 100, 250)
encoder_layers <- c(200, 100)
decoder_layers <- c(100, 200)
latent_dim <- 30


optimizer <- optimizer_adam()
loss <- loss_mean_squared_error

new_model_parc <- create_and_compile_ae(width, 
                                        values_per_input_voxel = 1, #num_classes + 1,
                                        encoder_layers, 
                                        latent_dim = latent_dim,
                                        decoder_layers, 
                                        values_per_output_voxel = num_classes,
                                        output_activation = "multivalued",
                                        clf = FALSE,
                                        resnet = FALSE,
                                        optimizer = optimizer, 
                                        loss = loss) 

summary(new_model_parc$model)

# train and evaluate
# Data generators
num_windows <- 15000
max_sub_epochs <- round(120000 / num_windows)
label_to_model <- scgm_labels
scale_type <- "none"

train_generator_array <- nifti_files_generator_sampling(x_files = mask_images[train_indices], 
                                                        y_files = NULL,
                                                        y_label = label_to_model,
                                                        categorize_output = FALSE,
                                                        num_classes = num_classes,
                                                        binarise = FALSE,
                                                        remap_classes = list(source = label_to_model, 
                                                                             target = seq(num_classes)),
                                                        minimum_sample_weight = NULL,
                                                        class_balance = FALSE,
                                                        scale = scale_type,
                                                        scale_y = scale_type,
                                                        width = width, 
                                                        num_windows = num_windows, 
                                                        max_sub_epochs = max_sub_epochs)

test_generator_array <- nifti_files_generator_sampling(x_files = mask_images[test_indices], 
                                                       y_files = NULL,
                                                       y_label = label_to_model,
                                                       categorize_output = FALSE,
                                                       num_classes = num_classes,
                                                       binarise = FALSE,
                                                       remap_classes = list(source = label_to_model, 
                                                                            target = seq(num_classes)),
                                                       minimum_sample_weight = NULL,
                                                       class_balance = FALSE,
                                                       scale = scale_type,
                                                       scale_y = scale_type,
                                                       width = width, 
                                                       num_windows = num_windows, 
                                                       max_sub_epochs = max_sub_epochs)

new_model_parc %>% fit_with_generator(generator = train_generator_array,
                                      steps_per_epoch = length(train_indices) * max_sub_epochs, 
                                      validation_data = test_generator_array,
                                      validation_steps = length(test_indices),
                                      epochs = 10,
                                      batch_size = min(5000, num_windows),
                                      keep_best = FALSE)

data <- test_generator_array()
X <- data[[1]]
Y <- new_model_parc$encoder %>% predict_on_batch(X)
Y_pred <- new_model_parc$decoder %>% predict_on_batch(Y)

Y_real <- data[[2]]
ind <- which(Y_real[, 1] != 0)[1]
ind <- 100

y <- Y_pred[ind, ]
# y_ <- matrix(y, nrow = num_classes + 1)
# res_pred <- array(apply(y_, 2, which.max) - 1, dim = c(width, width, width))
res_pred <- array(y, dim = c(width, width, width))
neurobase::ortho2(res_pred)

y <- Y_real[ind, ]
# y_ <- matrix(y, nrow = num_classes + 1)
# res_real <- array(apply(y_, 2, which.max) - 1, dim = c(width, width, width))
res_real <- array(y, dim = c(width, width, width))
neurobase::ortho2(res_real)


####                   Inference                        ####

new_index <- sample(test_indices, size = 1)

new_img <- neurobase::readnii(templates[new_index])
ground_truth <- neurobase::readnii(mask_images[new_index])

new_seg <- new_model_parc %>% infer_volume(V = as.array(ground_truth), 
                                           scale = scale_type,
                                           binarise = FALSE)
gc()

new_seg <- round(new_seg)
new_seg[new_seg < 0] <- 0

####                   Visualization                    ####

require(neurobase)

ground_truth[ground_truth == 0] <- NA
new_seg[new_seg == 0] <- NA
orthographic(ground_truth)
orthographic(new_seg)

ortho_diff(new_img, 
           pred = niftiarr(arr = new_seg, new_img), 
           roi = niftiarr(arr = ground_truth, new_img), 
           addlegend = TRUE)

gc()
