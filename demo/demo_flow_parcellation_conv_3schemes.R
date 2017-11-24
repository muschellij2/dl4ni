##%######################################################%##
#                                                          #
####       Example of Flow for Brain Parcellation       ####
####                   Convolutional                    ####
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

# Schemes for all networks in the flow
width <- 32

scheme_small <- DLscheme$new()

scheme_small$add(width = width,
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
                  optimizer = "adadelta",
                  scale = "none",
                  scale_y = "none")

scheme_small$add(memory_limit = "3G")

scheme_medium <- DLscheme$new()

scheme_medium$add(width = width,
                  only_convolutionals = TRUE,
                  output_width = width,
                  num_features = 3,
                  vol_layers_pattern = segnet(depth = as.integer(log2(width) - 1), 
                                              mode = "convolutional", 
                                              initial_filters = 8),
                  vol_dropout = 0,
                  feature_layers = list(),
                  feature_dropout = 0,
                  common_layers = list(),
                  common_dropout = 0,
                  last_hidden_layers = list(),
                  optimizer = "adadelta",
                  scale = "none",
                  scale_y = "none")

scheme_medium$add(memory_limit = "3G")


scheme_big <- DLscheme$new()

scheme_big$add(width = width,
                  only_convolutionals = TRUE,
                  output_width = width,
                  num_features = 3,
                  vol_layers_pattern = segnet(depth = as.integer(log2(width) - 1), 
                                              mode = "convolutional", 
                                              initial_filters = 32),
                  vol_dropout = 0,
                  feature_layers = list(),
                  feature_dropout = 0,
                  common_layers = list(),
                  common_dropout = 0,
                  last_hidden_layers = list(),
                  optimizer = "adadelta",
                  scale = "none",
                  scale_y = "none")

scheme_big$add(memory_limit = "3.5G")


##%######################################################%##
#                                                          #
####                   Flow Creation                    ####
#                                                          #
##%######################################################%##

# Create new flow
flow <- DLflow$new(name = "parcellation", inputs = c("T1"))

# Scale the T1 image
flow$add(what = scale_z, 
         inputs = list("T1"), 
         output = "T1_scaled")

# Starting from a T1, add a trainable model which computes the brain_mask
flow$add(what = scheme_small, 
         inputs = list("T1_scaled"),
         output = "brain_mask")

# To compute the brain extracted image, we multiply the T1 and the brain_mask
flow$add(what = function(T1, brain_mask) {T1 * brain_mask}, 
         output = "only_brain")

# Scale the brain extracted image
flow$add(what = scale_z, 
         inputs = list("only_brain"), 
         output = "only_brain_scaled")

# Starting form the brain extracted image ("only_brain"), add a trainable model which computes the 
# segmentation
flow$add(what = scheme_small,
         inputs = list("only_brain_scaled"),
         output = "segmentation")

# Using brain extracted and scaled image ("only_brain_scaled") and the segmentation, add a trainable model
# to compute the parcellation
cortex <- c(6, 45, 630:3000)
scgm_labels <- c(10, 11, 12, 13, 17, 18, 49:54)
spinal_cord_labels <- 16
ventricles_labels <- c(4, 5, 14, 15, 24, 43, 44, 72)
flow$add(what = scheme_big, 
         inputs = list("only_brain_scaled", "segmentation"),
         output = "parcellation", 
         subset = list(subset_classes = scgm_labels,
                       unify_classes = list(cortex, 
                                            spinal_cord_labels, 
                                            ventricles_labels)))

##%######################################################%##
#                                                          #
####                   Flow Plotting                    ####
#                                                          #
##%######################################################%##

flow$plot()

##%######################################################%##
#                                                          #
####                   Model Training                   ####
#                                                          #
##%######################################################%##

# Let's train the different models:
# Define the training sets:

# First, the brain_mask
problem <- "brain_extraction"
info_bet <- problem %>% get_problem_info()

# Now, segmentation
problem <- "segmentation2"
info_seg <- problem %>% get_problem_info()

# To end, parcellation
problem <- "parcellation"
info_parc <- problem %>% get_problem_info()

# Train BET
flow$train(output = "brain_mask", 
           input_filenames = info_bet$inputs, 
           output_filenames = info_bet$outputs, 
           epochs = 30)

# Train segmentation
flow$train(output = "segmentation", 
           input_filenames = list("only_brain" = info_seg$inputs$T1),
           output_filenames = info_seg$outputs, 
           epochs = 30)

# Train parcellation
flow$train(output = "parcellation", 
           input_filenames = list("only_brain" = info_parc$inputs$T1),
           output_filenames = info_parc$outputs, 
           epochs = 30)


##%######################################################%##
#                                                          #
####                     Save Flow                      ####
#                                                          #
##%######################################################%##

flow$save(path = system.file("models", package = "dl4ni.models"))

##%######################################################%##
#                                                          #
####               Execute on Test Image                ####
#                                                          #
##%######################################################%##

# Starting from original image
file <- info_bet$inputs$T1[1]
result <- flow %>% execute_flow(inputs = list(T1 = file), 
                                desired_outputs = c("brain_mask", "segmentation", "parcellation"))

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

# Starting from betted image
file <- info_seg$inputs$T1[1]
result <- flow %>% execute_flow(inputs = list(only_brain = file), 
                                desired_outputs = c("segmentation", "parcellation"))

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

