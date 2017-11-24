##%######################################################%##
#                                                          #
####           Example of Flow for T1 to FLAIR          ####
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
width <- 32

scheme_bigger <- DLscheme$new()

scheme_bigger$add(width = width,
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

scheme_bigger$add(memory_limit = "3G")

##%######################################################%##
#                                                          #
####                   Flow Creation                    ####
#                                                          #
##%######################################################%##

# Create new flow
flow <- DLflow$new(name = "t1_flair", inputs = c("T1"))

# Scale the T1 image
flow$add(what = scale_z, 
         inputs = list("T1"), 
         output = "T1_scaled")

# Starting from a T1, add a trainable model which computes the brain_mask
flow$add(what = scheme_bigger, 
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
flow$add(what = scheme_bigger,
         inputs = list("only_brain_scaled"),
         output = "segmentation")

# Using brain extracted and scaled image ("only_brain_scaled") and the segmentation, add a trainable model
flow$add(what = scheme_bigger, 
         inputs = list("only_brain_scaled", "segmentation"),
         output = "flair")


##%######################################################%##
#                                                          #
####                   Flow Plotting                    ####
#                                                          #
##%######################################################%##

flow$plot()

##%######################################################%##
#                                                          #
####                      New flow                      ####
#                                                          #
##%######################################################%##

flow <- DLflow$new(name = "t1_flair", inputs = c("T1"))
flow$load("parcellation.zip")
flow <- flow$subset("segmentation")

# Using brain extracted and scaled image ("only_brain_scaled") and the segmentation, add a trainable model
flow$add(what = scheme_bigger, 
         inputs = list("only_brain_scaled", "segmentation"),
         output = "flair")

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

# To end, t1 to flair
problem <- "t1_flair"
info_flair <- problem %>% get_problem_info()

# Train BET
flow$train(output = "brain_mask", 
           input_filenames = info_bet$inputs, 
           output_filenames = info_bet$outputs, 
           epochs = 15)

# Train segmentation
flow$train(output = "segmentation", 
           input_filenames = info_seg$inputs,
           given_input = list("only_brain" = info_seg$inputs$T1),
           output_filenames = info_seg$outputs, 
           epochs = 15)

# Train T1 to FLAIR
flow$train(output = "flair", 
           input_filenames = info_flair$inputs,
           given_input = list("only_brain" = info_flair$inputs$T1),
           output_filenames = info_flair$outputs, 
           epochs = 15)


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
file <- info_bet$inputs$T1[5]
result <- flow$execute(inputs = list(T1 = file), 
                       desired_outputs = c("brain_mask", "segmentation", "flair"))

flair_img <- result$flair
result$flair <- NULL

original_image <- read_nifti_to_array(file)
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



