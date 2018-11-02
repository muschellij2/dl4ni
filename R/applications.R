##%######################################################%##
#                                                          #
####          Import External Model Functions           ####
#                                                          #
##%######################################################%##

get_applications_folder <- function() {
  
  system.file("applications", package = "dl4ni")
  
}

get_custom_objects <- function() {
  
  keras_contrib <- reticulate::import("keras_contrib")
  
  assign(x = "InstanceNormalization", 
         value = keras_contrib$layers$InstanceNormalization, 
         pos = parent.frame())
  
}

##%######################################################%##
#                                                          #
####                    Applications                    ####
#                                                          #
##%######################################################%##

app_tumor_detection <- function() {
  
  require(keras)
  
  model_path <- file.path(get_applications_folder(), 
                          "isensee_2017_model.h5")
  
  get_custom_objects()
  custom_objects <- c("weighted_dice_coefficient_loss" = 
                        weighted_dice_coefficient_loss)
  
  model <- load_model_hdf5(model_path, custom_objects = custom_objects)
  
  suppressWarnings(my_model <- model %>% recreate_model(scale = "none", 
                                                        categorize_output = FALSE,
                                                        only_convolutionals = TRUE,
                                                        memory_limit = "10GB"))
  
  return(my_model)
  
}

app_lesion_detection <- function() {
  
  require(keras)
  
  model_path <- file.path(get_applications_folder(), 
                          "lesion_detection_model.h5")
  
  get_custom_objects()

  model <- load_model_hdf5(model_path)
  
  my_model <- model %>% recreate_model(categorize_output = TRUE, 
                                       num_classes = 2)
  
  return(my_model)
  
}

app_dgm_segmentation <- function() {
  
  require(keras)
  
  model_path <- file.path(get_applications_folder(), 
                          "zl376_segDGM_CNN.h5")
  
  get_custom_objects()
  
  model <- load_model_hdf5(model_path)
  
  my_model <- model %>% recreate_model(categorize_output = TRUE, 
                                       num_classes = 2)
  
  return(my_model)
  
}

applications <- list()
applications$app_tumor_detection  <- app_tumor_detection
applications$app_lesion_detection <- app_lesion_detection
applications$app_dgm_segmentation <- app_dgm_segmentation

available_applications <- function() {
  
  names(applications)
  
}

##%######################################################%##
#                                                          #
####        Convert from keras model to DLmodel         ####
#                                                          #
##%######################################################%##

recreate_model <- function(model, ...) {
  
  args <- list(...)
  
  # Input and output shapes
  input_shape <- model$input_shape %>% unlist()
  output_shape <- model$output_shape %>% unlist()
  
  # Windows dimensions and channel-first/-last detection
  input4D <- FALSE
  if (length(input_shape > 3)) {
    
    # 4D input
    input4D <- TRUE
    idx_channels <- which(input_shape <= 5)
    
    width <- unique(input_shape[-idx_channels])[1]

  } else {
    
    # 3D input
    width <- unique(input_shape)[1]

  }
  
  output4D <- FALSE
  if (length(output_shape > 3)) {
    
    # 4D output
    output4D <- TRUE
    idx_channels <- which(output_shape <= 5)
    
    output_width <- unique(output_shape[-idx_channels])[1]
    
  } else {
    
    # 3D output
    output_width <- unique(output_shape)[1]
    
  }
  
  if (idx_channels == 1) channels <- "first" else channels <- "last"
  
  args$output_width <- output_width
  args$width <- width
  args$optimizer <- model$optimizer %>% 
    parse_optimizer()
  args$path <- "volumes"
  args$regularize <- NULL
  args$input4D <- input4D
  args$output4D <- output4D
  args$channels <- channels
  
  hyperparameters <- do.call(what = define_config,
                             args = args)
  
  my_model <- DLmodel$new()
  
  my_model$update(model = model,
                  width = width,
                  hyperparameters = hyperparameters)
  
  return(my_model)
  
}
