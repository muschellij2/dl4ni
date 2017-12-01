scheme_segnet <- function(width, initial_filters = 32, full_depth = TRUE) {
  
  depth <- as.integer(log2(width) - 1)
  
  if (full_depth)
    depth <- depth + 1
  
  scheme <- DLscheme$new()
  
  scheme$add(width = width,
             only_convolutionals = TRUE,
             output_width = width,
             num_features = 3,
             vol_layers_pattern = segnet(depth = depth, 
                                         mode = "convolutional", 
                                         initial_filters = initial_filters),
             vol_dropout = 0,
             feature_layers = list(),
             feature_dropout = 0,
             common_layers = list(),
             common_dropout = 0,
             last_hidden_layers = list(),
             optimizer = "adadelta",
             scale = "none",
             scale_y = "none")
  
  return(scheme)
  
}

scheme_unet <- function(width, initial_filters = 32, full_depth = TRUE) {
  
  depth <- as.integer(log2(width) - 1)
  
  if (full_depth)
    depth <- depth + 1
  
  scheme <- DLscheme$new()
  
  scheme$add(width = width,
             only_convolutionals = TRUE,
             output_width = width,
             num_features = 3,
             vol_layers_pattern = unet(depth = depth, 
                                       mode = "convolutional", 
                                       initial_filters = initial_filters),
             vol_dropout = 0,
             feature_layers = list(),
             feature_dropout = 0,
             common_layers = list(),
             common_dropout = 0,
             last_hidden_layers = list(),
             optimizer = "adadelta",
             scale = "none",
             scale_y = "none")
  
  return(scheme)
  
}

scheme_meshnet <- function(width, initial_filters = 32, num_blocks = 7, dropout = 0.25) {
  
  scheme <- DLscheme$new()
  
  scheme$add(width = width,
             only_convolutionals = TRUE,
             output_width = width,
             num_features = 3,
             vol_layers_pattern = meshnet(num_filters = initial_filters, 
                                          num_blocks = num_blocks, 
                                          dropout = dropout, 
                                          out_filters = 0),
             vol_dropout = 0,
             feature_layers = list(),
             feature_dropout = 0,
             common_layers = list(),
             common_dropout = 0,
             last_hidden_layers = list(),
             optimizer = "adadelta",
             scale = "none",
             scale_y = "none")
  
  return(scheme)
  
}
