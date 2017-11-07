##%######################################################%##
#                                                          #
####       Example of Flow for Brain Parcellation       ####
#                                                          #
##%######################################################%##

require(neurobase)
devtools::load_all("../dl4ni.data/")
load_keras()

##%######################################################%##
#                                                          #
####             Network Scheme Definition              ####
#                                                          #
##%######################################################%##

# Basic scheme for all networks in the flow
scheme_bigger <- list(width = 7,
                      output_width = 3,
                      num_features = 3,
                      vol_layers_pattern = list(clf(hidden_layers = list(dense(300), dense(200), dense(200), dense(100)))),
                      vol_dropout = 0.1,
                      feature_layers = list(clf(hidden_layers = list(dense(10), dense(10)))),
                      feature_dropout = 0.15,
                      common_layers = list(clf(hidden_layers = list(dense(300), dense(250), dense(100)))),
                      common_dropout = 0.1,
                      last_hidden_layers = list(dense(30), dense(20)),
                      optimizer = keras::optimizer_adadelta(),
                      scale = "none",
                      scale_y = "none")


##%######################################################%##
#                                                          #
####                   Flow Creation                    ####
#                                                          #
##%######################################################%##

# Create new flow
flow <- create_flow(name = "t1_flair", inputs = c("T1"))

# Scale the T1 image
flow %>% add_process(proc = scale_z, 
                     inputs = list("T1"), 
                     output = "T1_scaled")

# Starting from a T1, add a trainable model which computes the brain_mask
flow %>% add_trainable_model(scheme = scheme_bigger, 
                             inputs = list("T1_scaled"),
                             output = "brain_mask")

# To compute the brain extracted image, we multiply the T1 and the brain_mask
flow %>% add_process(proc = function(T1, brain_mask) {T1 * brain_mask}, 
                     output = "only_brain")

# Scale the brain extracted image
flow %>% add_process(proc = scale_z, 
                     inputs = list("only_brain"), 
                     output = "only_brain_scaled")

# Starting form the brain extracted image ("only_brain"), add a trainable model which computes the 
# segmentation
flow %>% add_trainable_model(scheme = scheme_bigger,
                             inputs = list("only_brain_scaled"),
                             output = "segmentation")

# Using brain extracted and scaled image ("only_brain_scaled") and the segmentation, add a trainable model
flow %>% add_trainable_model(scheme = scheme_bigger, 
                             inputs = list("only_brain_scaled", "segmentation"),
                             output = "flair")


##%######################################################%##
#                                                          #
####                   Flow Plotting                    ####
#                                                          #
##%######################################################%##

flow %>% plot_flow()

##%######################################################%##
#                                                          #
####                      New flow                      ####
#                                                          #
##%######################################################%##


flow <- load_flow("parcellation.zip")
flow <- flow %>% subset_flow("segmentation")

# Using brain extracted and scaled image ("only_brain_scaled") and the segmentation, add a trainable model
flow %>% add_trainable_model(scheme = scheme_bigger, 
                             inputs = list("only_brain_scaled", "segmentation"),
                             output = "flair")

flow %>% plot_flow()

##%######################################################%##
#                                                          #
####                   Model Training                   ####
#                                                          #
##%######################################################%##

# Let's train the different models:
# Define the training sets:

# First, the brain_mask
problem <- "brain_extraction"
info_bet <- problem %>% get_problem_info(num_subjects = 15)

# Now, segmentation
problem <- "segmentation2"
info_seg <- problem %>% get_problem_info(num_subjects = 15)

# To end, t1 to flair
problem <- "t1_flair"
info_flair <- problem %>% get_problem_info(num_subjects = 15)

# Train BET
flow %>% train_output(output = "brain_mask", 
                      input_filenames = info_bet$inputs, 
                      output_filenames = info_bet$outputs, 
                      epochs = 5)

# Train segmentation
flow %>% train_output(output = "segmentation", 
                      input_filenames = info_seg$inputs,
                      given_input = list("only_brain" = info_seg$inputs$T1),
                      output_filenames = info_seg$outputs, 
                      epochs = 5)

# Train T1 to FLAIR
flow %>% train_output(output = "flair", 
                      input_filenames = info_flair$inputs,
                      given_input = list("only_brain" = info_flair$inputs$T1),
                      output_filenames = info_flair$outputs, 
                      epochs = 5,
                      mode = "faster")


##%######################################################%##
#                                                          #
####                     Save Flow                      ####
#                                                          #
##%######################################################%##

flow %>% save_flow(path = system.file("models", package = "dl4ni.models"), file_prefix = flow$name)

##%######################################################%##
#                                                          #
####               Execute on Test Image                ####
#                                                          #
##%######################################################%##

# Starting from original image
file <- info_bet$inputs$T1[5]
result <- flow %>% execute_flow(inputs = list(T1 = file), 
                                desired_outputs = c("brain_mask", "segmentation", "flair"))

flair_img <- result$flair
result$flair <- NULL

original_image <- readnii(file)
ortho_plot(x = original_image, interactiveness = FALSE, text = "Original Image")
for (img in seq_along(result)) {
  
  num_classes <- length(unique(as.vector(result[[img]])))
  col.y <- scales::alpha(colour = scales::hue_pal()(num_classes), alpha = 0.45)
  if (names(result)[img] == "segmentation")
    col.y <- scales::alpha(colour = scales::viridis_pal()(num_classes), alpha = 0.25)
  
  ortho_plot(x = original_image, 
             y = result[[img]], 
             col.y = col.y, 
             interactiveness = FALSE, 
             text = paste0("Predicted: ", names(result)[img]))
  
}

ortho_plot(x = flair_img, 
           col.y = col.y, 
           interactiveness = FALSE, 
           text = paste0("Predicted: FLAIR"))


# Starting from segmented image
file <- info_seg$inputs$T1[1]
result <- flow %>% execute_flow(inputs = list(only_brain = file), 
                                desired_outputs = c("segmentation", "flair"))

original_image <- readnii(file)
ortho_plot(x = original_image, interactiveness = FALSE, text = "Original Image")
for (img in seq_along(result)) {
  
  num_classes <- length(unique(as.vector(result[[img]])))
  col.y <- scales::alpha(colour = scales::hue_pal()(num_classes), alpha = 0.45)
  if (names(result)[img] == "segmentation")
    col.y <- scales::alpha(colour = scales::viridis_pal()(num_classes), alpha = 0.25)
  
  ortho_plot(x = original_image, 
             y = result[[img]], 
             col.y = col.y, 
             interactiveness = FALSE, 
             text = paste0("Predicted: ", names(result)[img]))
  
}

