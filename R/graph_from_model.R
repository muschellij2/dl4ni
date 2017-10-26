#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param .model    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
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
  
  if (!require(igraph))
    stop("Package 'igraph' not installed.")
  
  if (inherits(.model, "DLmodel")) {
    
    model <- .model$model
    
    model_structure <- model %>% 
      keras::model_to_json() %>% 
      jsonlite::fromJSON()
    
    layers_name <- model_structure$config$layers$name
    layers_type <- model_structure$config$layers$class_name
    
    num_layers <- length(layers_name)
    
    adj_matrix <- matrix(0, nrow = num_layers, ncol = num_layers)
    all_parents <- matrix(0, nrow = num_layers, ncol = num_layers)
    colnames(adj_matrix) <- layers_name
    rownames(adj_matrix) <- layers_name
    
    hidden_types <- c("BatchNormalization", "Activation", "Dropout", "Concatenate")
    sizes <- model_structure$config$layers$config$units
    
    # All layers but the last one
    for (layer_id in seq(num_layers - 1)) {
      
      if (!(layers_type[layer_id] %in% hidden_types)) {
        
        inbound_nodes <- model_structure$config$layers$inbound_nodes[[layer_id]]
        
        if (length(inbound_nodes) > 0) {
          
          parents <- layer_id %>% .search_parents(inbound_nodes = model_structure$config$layers$inbound_nodes,
                                                  layers_name = layers_name, 
                                                  layers_type = layers_type, 
                                                  hidden_types = hidden_types)
          
          
          adj_matrix[parents, layer_id] <- 1

          parents <- layer_id %>% .search_parents(inbound_nodes = model_structure$config$layers$inbound_nodes,
                                                  layers_name = layers_name, 
                                                  layers_type = layers_type, 
                                                  hidden_types = hidden_types, 
                                                  mode = "all")

          all_parents[parents, layer_id] <- 1
          
        }
        
      }
      
    }
    
    # Last layer
    parents <- num_layers %>% .search_parents(inbound_nodes = model_structure$config$layers$inbound_nodes,
                                            layers_name = layers_name, 
                                            layers_type = layers_type, 
                                            hidden_types = hidden_types)
    
    
    adj_matrix[parents, num_layers] <- 1
    
    parents <- num_layers %>% .search_parents(inbound_nodes = model_structure$config$layers$inbound_nodes,
                                            layers_name = layers_name, 
                                            layers_type = layers_type, 
                                            hidden_types = hidden_types, 
                                            mode = "all")
    
    all_parents[parents, num_layers] <- 1
    
    which_to_remove <- which(colSums(adj_matrix) == 0)
    input_layers <- which(layers_type == "InputLayer")
    which_to_remove <- setdiff(which_to_remove, input_layers)
    
    adj_matrix <- adj_matrix[-which_to_remove, -which_to_remove]
    all_parents <- all_parents[-which_to_remove, -which_to_remove]
    
    new_names <- layers_name[-which_to_remove]
    
    input_layer_names <- new_names[grep(new_names, pattern = "input_")]
    input_layers_order <- order(as.numeric(gsub(pattern = "input_", replacement = "", x = input_layer_names)))
    feat_layer_index <- input_layers_order[1]
    new_names[new_names == input_layer_names[feat_layer_index]] <- "Feature Input"
    other_inputs <- grep(new_names, pattern = "input_")
    
    for (i in seq_along(other_inputs)) {
      
      old_name <- input_layer_names[input_layers_order[i + 1]]
      new_names[new_names == old_name] <- paste0("Volume Input ", i)
      
    }
    
    input_layers <- which(layers_type[-which_to_remove] == "InputLayer")
    num_inputs <- length(input_layers)
    relation_to_inputs <- all_parents[input_layers, ]
    
    first_common_layer <- which(colSums(relation_to_inputs) == num_inputs)[1]
    new_names[first_common_layer] <- "Concatenate Paths"
    
    new_names[length(new_names)] <- "Output"
    
    sizes <- model_structure$config$layers$config$units[-which_to_remove]
    sizes[length(sizes)] <- config$output_width ^ 3
    sizes[new_names == "Feature Input"] <- config$num_features
    sizes[grepl(pattern = "Volume", x = new_names)] <- config$width ^ 3
    colnames(adj_matrix) <- new_names
    rownames(adj_matrix) <- new_names
    g <- igraph::graph.adjacency(adj_matrix, mode = "directed")
    V(g)$type <- layers_type[-which_to_remove]
    V(g)$size <- sizes
    V(g)$name <- new_names
    
    return(g)
    
  } else {
    
    return(invisible(NULL))
    
  }
  
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
#' @param g    (name) PARAM_DESCRIPTION
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
plot_graph <- function(g) {
  
  num_types <- length(unique(V(g)$type))
  colors <- scales::alpha(colour = scales::hue_pal()(num_types), alpha = 0.85)
  V(g)$color <- colors[as.numeric(as.factor(V(g)$type))]
  
  V(g)$size <- V(g)$size / max(V(g)$size, na.rm = TRUE) * 25 + 3
  
  L <- igraph::layout_with_sugiyama(g, hgap = 30)
  coords <- L$layout
  levels <- coords[, 2]
  highest_level <- max(levels)
  knot <- highest_level - which.min(rev(table(levels))) + 1
  # # coords_highest_level <- coords[coords[, 2] == highest_level, 1]
  minx <- min(coords[, 1])
  maxx <- max(coords[, 1])

  for (current_level in seq(highest_level, knot + 1, by = -1)) {

    coords_level <- coords[coords[, 2] == current_level, 1]

    if (length(coords_level) > 1) {
      
      s <- order(coords_level)
      coef <- (seq_along(coords_level) - 1) / (length(coords_level) - 1)
      
      new_coords <- minx + coef[s] * (maxx - minx)
      coords[coords[, 2] == current_level, 1] <- new_coords

      # coords_level <- coords_level / max(coords_level)
      # coords_level <- coords_level - mean(coords_level)
      # coords_level <- exp(coords_level)
      # coords_level <- (coords_level - min(coords_level)) / (max(coords_level) - min(coords_level))
      # coords_level <- minx + coords_level * (maxx - minx)
      # coords[coords[, 2] == current_level, 1] <- coords_level

    } else {

      break

    }

  }
  
  L$layout <- norm_coords(coords, ymin = -1, ymax = 1, xmin = -1, xmax = 1)
  plot(g, edge.arrow.size = .4, vertex.label = NA, edge.curved = 0, layout = L$layout, rescale = FALSE)
  
}
