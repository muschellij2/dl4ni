create_flow <- function(inputs = list()) {
  
  require(igraph)
  
  # A flow is an environment
  flow <- new.env()
  
  # List of flow inputs
  flow$inputs <- inputs
  
  # List of flow processes (both models and functions)
  flow$processes <- list()
  
  # Create graph of dependencies
  flow$graph <- igraph::make_empty_graph(directed = TRUE)
  if (length(inputs) > 0) {
    
    flow$graph <- igraph::add_vertices(nv = length(inputs), name = inputs, type = rep("Input", length(inputs)))

  }
  
  # List of all possible outputs of the flow
  flow$outputs <- inputs
  
  class(flow) <- "DLflow"
  return(flow)
  
}

add_process <- function(flow, proc, inputs = list(), output) {
  
  # Basic checks
  stopifnot(inherits(flow, "DLflow"))
  stopifnot(inherits(proc, "DLmodel") | inherits(proc, "function"))
  output <- as.character(output)
  
  type <- "DLmodel"
  if (inherits(proc, "function")) type <- "function"
  
  # Add a node to the graph, with edges from its inputs to it.
  flow$graph <- flow$graph %>% igraph::add_vertices(nv = 1, name = output, type = type)
  new_vertex_idx <- length(V(flow$graph))
  
  if (length(inputs) > 0) {
    
    input_ids <- match(inputs, flow$outputs)
    
    flow$graph <- flow$graph %>% igraph::add_edges(edges = as.vector(rbind(input_ids, new_vertex_idx)))
    
  }

  # Add the model to the list of flow models
  flow$processes[[output]] <- proc
  
  return(invisible(flow))
  
}

plot_flow <- function(flow, interactive = FALSE) {
  
  stopifnot(inherits(flow, "DLflow"))
  
  # Define color of each node as its type
  num_types <- length(unique(V(flow$graph)$type))
  colors <- scales::alpha(colour = scales::hue_pal()(num_types), alpha = 0.85)
  V(flow$graph)$color <- colors[as.numeric(as.factor(V(flow$graph)$type))]
  
  # Graph layout
  L <- igraph::layout_with_sugiyama(flow$graph, hgap = 30)
  coords <- L$layout
  
  L$layout <- norm_coords(coords, ymin = -1, ymax = 1, xmin = -1, xmax = 1)
  
  # Graph plot
  if (interactive && require(htmlwidgets) && require(threejs)) {
    
    threejs::graphjs(flow$graph, main = "Network", bg = "gray10", edge.arrow.size = .4, 
                     vertex.label = NA, edge.curved = 0, layout = cbind(L$layout, 0), showLabels = TRUE, 
                     stroke = FALSE, curvature = 0.1, attraction = 0.9, repulsion = 0.8, opacity = 0.9)
    
  } else {
    
    plot(flow$graph, edge.arrow.size = .4, vertex.label = TRUE, edge.curved = 0, layout = L$layout, rescale = FALSE)
    
  }
  
  
}

execute_flow <- function(flow, inputs = list(), desired_outputs = NULL) {
  
  stopifnot(inherits(flow, "DLflow"))
  
  # Check that inputs is a named list of files and that all of them exist
  
  # Define which parts of the flow must be processed
  
  
}

save_flow <- function(flow, path = tempdir()) {
  
  # Models must be saved apart.
  
}

load_flow <- function(filename) {
  
  
  
}

train_flow <- function(flow, epochs = 10, max_sub_epochs = 5) {
  
  stopifnot(inherits(flow, "DLflow"))
  
  
}
