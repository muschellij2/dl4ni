
##%######################################################%##
#                                                          #
####           Example For Brain Parcellation 2         ####
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
vol_layers <- c(200, 100)
common_layers <- c(100, 250)
individual_layers <- c(50)
number_of_outputs <- width ^ 3
num_classes <- length(scgm_labels)


optimizer <- optimizer_nadam()
loss <- loss_mean_squared_error #loss_binary_crossentropy #

new_model_parc <- create_and_compile_model_categorical_output(width, 
                                                              vol_layers = vol_layers,
                                                              common_layers = common_layers, 
                                                              individual_layers = individual_layers,
                                                              number_of_outputs = number_of_outputs,
                                                              num_classes = num_classes,
                                                              clf = TRUE,
                                                              resnet = FALSE,
                                                              optimizer = optimizer,
                                                              loss = loss)
summary(new_model_parc$model)

# train and evaluate
# Data generators
num_windows <- 30000
max_sub_epochs <- round(120000 / num_windows)
label_to_model <- scgm_labels
scale_type <- "z"

train_generator_array <- nifti_files_generator_sampling(x_files = templates[train_indices], 
                                                        y_files = mask_images[train_indices],
                                                        y_label = label_to_model, 
                                                        binarise = FALSE,
                                                        categorize_output = FALSE,
                                                        num_classes = num_classes,
                                                        remap_classes = list(source = label_to_model, 
                                                                             target = seq(num_classes)),
                                                        only_central_voxel = FALSE,
                                                        minimum_sample_weight = NULL,
                                                        class_balance = FALSE,
                                                        scale = scale_type,
                                                        scale_y = "none",
                                                        width = width, 
                                                        num_windows = num_windows, 
                                                        max_sub_epochs = max_sub_epochs)

test_generator_array <- nifti_files_generator_sampling(x_files = templates[test_indices], 
                                                       y_files = mask_images[test_indices],
                                                       y_label = label_to_model, 
                                                       binarise = FALSE,
                                                       categorize_output = FALSE,
                                                       num_classes = num_classes,
                                                       remap_classes = list(source = label_to_model, 
                                                                            target = seq(num_classes)),
                                                       only_central_voxel = FALSE,
                                                       minimum_sample_weight = NULL,
                                                       class_balance = FALSE,
                                                       scale = scale_type,
                                                       scale_y = "none",
                                                       width = width, 
                                                       num_windows = num_windows, 
                                                       max_sub_epochs = max_sub_epochs)
  

new_model_parc %>% fit_with_generator(generator = train_generator_array,
                                      steps_per_epoch = length(train_indices) * max_sub_epochs, 
                                      validation_data = test_generator_array,
                                      validation_steps = length(test_indices) * max_sub_epochs,
                                      epochs = 20,
                                      batch_size = min(num_windows, 2000),
                                      keep_best = FALSE)

####                   Inference                        ####

new_index <- sample(test_indices, size = 1)

new_img <- neurobase::readnii(templates[new_index])
ground_truth <- neurobase::readnii(mask_images[new_index])
ground_truth[!(ground_truth %in% label_to_model)] <- 0
new_seg <- new_model_parc %>% infer_volume(V = as.array(new_img), 
                                           scale = scale_type,
                                           max_batch_size = 50000)
gc()

# new_seg <- round(new_seg)
# new_seg[new_seg < 0] <- 0

####                   Visualization                    ####

require(neurobase)

# ground_truth[ground_truth == 0] <- NA
# new_seg[new_seg == 0] <- NA

target_dims <- dim(new_img)
margin <- round(0.5 * (width + 1))
res <- 0 * new_seg
res[seq(margin, target_dims[1] - margin + 1), 
    seq(margin, target_dims[2] - margin + 1), 
    seq(margin, target_dims[3] - margin + 1)] <- new_seg[seq(1, target_dims[1] - width + 1), 
                                                         seq(1, target_dims[2] - width + 1), 
                                                         seq(1, target_dims[3] - width + 1)]
remap_classes <- list(source = label_to_model, target = seq(num_classes))

res_ <- res * 0
for (i in seq_along(remap_classes$source)) {
  
  res_[res == remap_classes$target[i]] <- remap_classes$source[i] 
  
}


# ortho2(as.array(ground_truth), text = "Ground Truth")
# ortho2(res_, text = "Predicted")
res <- res_
res[res == 0] <- NA

# col.y <- scales::alpha(colour = scales::brewer_pal(type = "qual", palette = 3)(num_classes), alpha = 0.35)
col.y <- scales::alpha(colour = scales::hue_pal()(num_classes), alpha = 0.35)
ortho2(new_img, ground_truth, col.y = col.y, text = "Ground Truth")

ortho2(new_img, niftiarr(arr = res, img = new_img), col.y = col.y, text = "Predicted")
# ortho2(new_seg)

# ortho_diff(new_img, 
#            pred = niftiarr(arr = new_seg, new_img), 
#            roi = niftiarr(arr = ground_truth, new_img), 
#            addlegend = TRUE)

gc()
