#' @title Create Graph from a Model
#'
#' @description This function creates a graph representation for a given model.
#'
#' @param .model    (\code{DLmodel}) Model for which to build the graph.
#'
#' @return An \code{igraph} object.
#'
#' @details DETAILS
#' @seealso 
#'  \code{\link[igraph]{graph.adjacency}}
#'  \code{\link[jsonlite]{fromJSON}}
#'  \code{\link[keras]{model_to_json}}
#'  
#' @export 
#' 
#' @importFrom igraph graph.adjacency
#' @importFrom jsonlite fromJSON
#' @importFrom keras model_to_json
#' @import igraph
#' 
graph_from_model <- function(.model) {
  
  # Check that igraph is installed
  if (!require(igraph))
    stop("Package 'igraph' not installed.")
  
  # Check the input class
  stopifnot(inherits(.model, "DLmodel"))
  
  model <- .model$model
  
  # Convert to JSON so we can inspect inner properties and relationships between layers
  model_structure <- model %>% 
    keras::model_to_json() %>% 
    jsonlite::fromJSON()
  
  # Layers names and types (Dense, Convolutional, Input)
  layers_name <- model_structure$config$layers$name
  layers_type <- model_structure$config$layers$class_name
  
  # Number of layers
  num_layers <- length(layers_name)
  
  # Adjacency matrix and matrix of all parent layers for each layer
  adj_matrix <- matrix(0, nrow = num_layers, ncol = num_layers)
  all_parents <- matrix(0, nrow = num_layers, ncol = num_layers)
  colnames(adj_matrix) <- layers_name
  rownames(adj_matrix) <- layers_name
  
  # Exclude these layer types from the graph
  hidden_types <- c("BatchNormalization", "Activation", "Dropout", "Concatenate")
  
  # "Size" of each layer
  sizes <- model_structure$config$layers$config$units
  
  # Loop over all layers to find connections, skipping
  # layers with type in "hidden_types".
  # We also compute all parents for each node.
  # We explicitly include the last layer, even if it's of
  # type "Concatenate", what means that the layer is the single
  # output of the model.
  for (layer_id in seq(num_layers)) {
    
    if (!(layers_type[layer_id] %in% hidden_types) | layer_id == num_layers) {
      
      # List of nodes which are used as inputs for current layer
      inbound_nodes <- model_structure$config$layers$inbound_nodes[[layer_id]]
      
      if (length(inbound_nodes) > 0) {
        
        # Compute direct parents, bypassing layers of "hidden_type"
        parents <- layer_id %>% .search_parents(inbound_nodes = model_structure$config$layers$inbound_nodes,
                                                layers_name = layers_name, 
                                                layers_type = layers_type, 
                                                hidden_types = hidden_types)
        
        
        adj_matrix[parents, layer_id] <- 1
        
        # Compute all parents, bypassing layers of "hidden_type"
        parents <- layer_id %>% .search_parents(inbound_nodes = model_structure$config$layers$inbound_nodes,
                                                layers_name = layers_name, 
                                                layers_type = layers_type, 
                                                hidden_types = hidden_types, 
                                                mode = "all")
        
        all_parents[parents, layer_id] <- 1
        
      }
      
    }
    
  }

  # Remove from the graph those nodes which are not used (due to beeing of class in "hidden_type")
  which_to_remove <- which(colSums(adj_matrix) == 0)
  
  # Input layers
  input_layers <- which(layers_type == "InputLayer")
  which_to_remove <- setdiff(which_to_remove, input_layers)
  
  # New adjacency matrix
  adj_matrix <- adj_matrix[-which_to_remove, -which_to_remove]
  all_parents <- all_parents[-which_to_remove, -which_to_remove]
  
  # Type of layers
  types <- layers_type[-which_to_remove]
  
  # Adopt new names for some layers
  new_names <- layers_name[-which_to_remove]
  
  # Names for input layers (feature input and volume inputs)
  input_layer_names <- model_structure$config$input_layers[, 1]
  input_layers_order <- order(as.numeric(gsub(pattern = "input_", replacement = "", x = input_layer_names)))
  feat_layer_index <- input_layers_order[1]
  new_names[new_names == input_layer_names[feat_layer_index]] <- "Feature Input"
  types[new_names == input_layer_names[feat_layer_index]] <- "Input_Feature"
  
  other_inputs <- grep(new_names, pattern = "input_")
  
  for (i in seq_along(other_inputs)) {
    
    old_name <- input_layer_names[input_layers_order[i + 1]]
    new_names[new_names == old_name] <- paste0("Volume Input ", i)
    types[new_names == paste0("Volume Input ", i)] <- "Input_Volume"
    
  }
  
  input_layers <- which(layers_type[-which_to_remove] == "InputLayer")
  num_inputs <- length(input_layers)
  relation_to_inputs <- all_parents[input_layers, ]
  
  # Knot where individual inputs merge
  if (num_inputs > 1) {

    first_common_layer <- which(colSums(relation_to_inputs) == num_inputs)[1]
    new_names[first_common_layer] <- "Concatenate Paths"
    types[first_common_layer] <- "Concatenate"
    
  }
  
  # Output layer(s)
  output_layers <- model_structure$config$output_layers[, 1] 
  new_names[match(output_layers, new_names)] <- paste0("Output ", seq_along(output_layers))
  types[match(paste0("Output ", seq_along(output_layers)), new_names)] <- "Output" 
  
  colnames(adj_matrix) <- new_names
  rownames(adj_matrix) <- new_names
  
  # Sizes of each layer
  sizes <- model_structure$config$layers$config$units[-which_to_remove]
  
  if (!.model$hyperparameters$multioutput) 
    sizes[length(sizes)] <- config$output_width ^ 3
  
  sizes[new_names == "Feature Input"] <- config$num_features
  sizes[grepl(pattern = "Volume", x = new_names)] <- config$width ^ 3
  
  # Build graph
  g <- igraph::graph.adjacency(adj_matrix, mode = "directed")
  V(g)$type <- types
  V(g)$size <- sizes
  V(g)$name <- new_names
  
  return(g)
  
}

.search_parents <- function(node_id, inbound_nodes, layers_name, layers_type, hidden_types, mode = c("first", "all")) {
  
  mode <- mode[1]
  
  parents <- c()
  
  if (length(inbound_nodes[[node_id]]) > 0) {
    
    in_nodes <- inbound_nodes[[node_id]][[1]]
    
    num_parents <- length(in_nodes)
    
    for (j in seq(num_parents)) {
      
      parent_name <- in_nodes[[j]][[1]][[1]]
      parent_id <- match(parent_name, layers_name)
      
      if (!(layers_type[parent_id] %in% hidden_types)) {
        
        parents <- c(parents, parent_id)
        
        if (mode == "all") {
          
          parents <- c(parents, .search_parents(parent_id, 
                                                inbound_nodes, 
                                                layers_name, 
                                                layers_type, 
                                                hidden_types, 
                                                mode = mode))
          
        }
        
      } else {
        
        parents <- c(parents, .search_parents(parent_id, 
                                              inbound_nodes, 
                                              layers_name, 
                                              layers_type, 
                                              hidden_types, 
                                              mode = mode))
        
      }
      
    }
    
  }
  
  return(unique(parents))
  
}


#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param g             (name) PARAM_DESCRIPTION
#' @param interactive   (logical) PARAM_DESCRIPTION, Default: TRUE
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso 
#'  \code{\link[igraph]{layout_with_sugiyama}}

#'  \code{\link[scales]{alpha}},\code{\link[scales]{hue_pal}}
#' @export 
#' @importFrom igraph layout_with_sugiyama
#' @importFrom scales alpha hue_pal
#' @import htmlwidgets
#' @import threejs
#' 
plot_graph <- function(g, interactive = FALSE) {
  
  num_types <- length(unique(V(g)$type))
  colors <- scales::alpha(colour = scales::hue_pal()(num_types), alpha = 0.85)
  V(g)$color <- colors[as.numeric(as.factor(V(g)$type))]
  
  V(g)$size <- V(g)$size / max(V(g)$size, na.rm = TRUE) * 25 + 3
  
  L <- igraph::layout_with_sugiyama(g, hgap = 30)
  coords <- L$layout
  levels <- coords[, 2]
  highest_level <- max(levels)
  knot <- highest_level - which.min(rev(table(levels))) + 1
  minx <- min(coords[, 1])
  maxx <- max(coords[, 1])
  
  if (highest_level > knot) {
    
    for (current_level in seq(highest_level, knot + 1, by = -1)) {
      
      coords_level <- coords[coords[, 2] == current_level, 1]
      
      if (length(coords_level) > 1) {
        
        s <- order(coords_level)
        coef <- (seq_along(coords_level) - 1) / (length(coords_level) - 1)
        
        new_coords <- minx + coef[s] * (maxx - minx)
        coords[coords[, 2] == current_level, 1] <- new_coords
        
      } else {
        
        break
        
      }
      
    }
    
  }
  
  L$layout <- norm_coords(coords, ymin = -1, ymax = 1, xmin = -1, xmax = 1)
  
  if (interactive && require(htmlwidgets) && require(threejs)) {
    
    threejs::graphjs(g, main = "Network", bg = "gray10", edge.arrow.size = .4, 
                     vertex.label = NA, edge.curved = 0, layout = cbind(L$layout, 0), showLabels = FALSE, 
                     stroke = FALSE, curvature = 0.1, attraction = 0.9, repulsion = 0.8, opacity = 0.9)
    
  } else {
    
    plot(g, edge.arrow.size = .4, vertex.label = NA, edge.curved = 0, layout = L$layout, rescale = FALSE)
    
  }
  
}
