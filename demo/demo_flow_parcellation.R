##%######################################################%##
#                                                          #
####       Example of Flow for Brain Parcellation       ####
#                                                          #
##%######################################################%##

require(neurobase)
require(dl4ni.data)
load_keras()

##%######################################################%##
#                                                          #
####             Network Scheme Definition              ####
#                                                          #
##%######################################################%##

# Basic scheme for all networks in the flow
scheme <- list(width = 7,
               output_width = 3,
               num_features = 3,
               vol_layers_pattern = list(dense(200), dense(100)),
               vol_dropout = 0.1,
               feature_layers = list(), #list(clf(hidden_layers = list(dense(10), dense(10))))
               feature_dropout = 0.15,
               common_layers = list(dense(250), dense(100)),
               common_dropout = 0.1,
               last_hidden_layers = list(dense(20), dense(10)),
               optimizer = keras::optimizer_nadam())


##%######################################################%##
#                                                          #
####                   Flow Creation                    ####
#                                                          #
##%######################################################%##


# Create new flow
flow <- create_flow(name = "parcellation", inputs = c("T1"))


# Starting from a T1, add a trainable model which computes the brain_mask
flow %>% add_trainable_model(using = scheme, 
                             output_template = get_problem_info("brain_extraction")$outputs[1], 
                             inputs = list("T1"),
                             output = "brain_mask")

# To compute the brain extracted image, we multiply the T1 and the brain_mask
flow %>% add_process(proc = function(T1, brain_mask) {T1 * brain_mask}, 
                     output = "only_brain")

# Starting form the brain extraced image ("only_brain"), add a trainable model which computes the 
# segmentation
flow %>% add_trainable_model(using = scheme, 
                             inputs = list("only_brain"),
                             output = "segmentation")

# Using brain extracted image ("only_brain") and the segmentation, add a trainable model
# to compute the parcellation
cortex <- c(6, 45, 630:3000)
scgm_labels <- c(10, 11, 12, 13, 17, 18, 49:54)
spinal_cord_labels <- 16
ventricles_labels <- c(4, 5, 14, 15, 24, 43, 44, 72)

flow %>% add_trainable_model(using = scheme, 
                             inputs = list("only_brain", "segmentation"),
                             output = "parcellation", 
                             subset = list(subset_classes = scgm_labels))


##%######################################################%##
#                                                          #
####                   Flow Plotting                    ####
#                                                          #
##%######################################################%##

flow %>% plot_flow()


##%######################################################%##
#                                                          #
####                   Model Training                   ####
#                                                          #
##%######################################################%##

# Let's train the different models:
# First, the brain_mask
problem <- "brain_extraction"
info_bet <- problem %>% get_problem_info(num_subjects = 5)
flow %>% train_output(output = "brain_mask", 
                      input_filenames = info_bet$inputs, 
                      output_filenames = info_bet$outputs, 
                      epochs = 1)


# Now, segmentation
problem <- "segmentation2"
info_seg <- problem %>% get_problem_info(num_subjects = 5)
flow %>% train_output(output = "segmentation", 
                      input_filenames = info_seg$inputs,
                      given_input = list("only_brain" = info_seg$inputs$T1),
                      output_filenames = info_seg$outputs, 
                      epochs = 1)

# To end, parcellation
problem <- "parcellation"
info_parc <- problem %>% get_problem_info(num_subjects = 5)
flow %>% train_output(output = "parcellation", 
                      input_filenames = info_parc$inputs,
                      given_input = list("only_brain" = info_parc$inputs$T1),
                      output_filenames = info_parc$outputs, 
                      epochs = 1)

##%######################################################%##
#                                                          #
####               Execute on Test Image                ####
#                                                          #
##%######################################################%##

file <- info_bet$inputs[1]
result <- flow %>% execute_flow(inputs = file, desired_outputs = c("only_brain", "segmentation", "parcellation"))

ortho_plot(x = readnii(file), interactiveness = FALSE, text = "Original Image")
for (img in seq_along(result)) {
  
  ortho_plot(x = result[[img]], interactiveness = FALSE, text = paste0("Predicted: ", names(result)[img]))
  
}

