#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param inputs    (call) PARAM_DESCRIPTION, Default: list()
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso 
#'  \code{\link[igraph]{make_empty_graph}},\code{\link[igraph]{add_vertices}}
#' @export 
#' @importFrom igraph make_empty_graph add_vertices
#' @import igraph
create_flow <- function(name = "", inputs = list()) {
  
  require(igraph)
  
  # A flow is an environment
  flow <- new.env()
  flow$name <- as.character(name)
  
  # List of flow inputs and outputs
  flow$inputs <- list()
  flow$outputs <- list()
  
  # List of flow processes (both models and functions)
  flow$processes <- list()
  flow$schemes <- list()
  flow$trained <- list()
  
  # List of pipelines to execute for each process and of required inputs
  flow$pipeline <- list()
  flow$required_inputs <- list()
  flow$inmediate_inputs <- list()
  
  # Create graph of dependencies
  flow$graph <- igraph::make_empty_graph(directed = TRUE)
  
  # Add inputs to the graph
  if (length(inputs) > 0) {
    flow$inputs <- inputs
    flow$graph <- flow$graph %>% igraph::add_vertices(nv = length(inputs), 
                                                      name = unlist(inputs), 
                                                      type = rep("Input", length(inputs)))
    
  }
  
  # List of all possible outputs of the flow
  flow$outputs <- unlist(inputs)
  
  class(flow) <- "DLflow"
  return(flow)
  
}

##%######################################################%##
#                                                          #
####                      PLOTTING                      ####
#                                                          #
##%######################################################%##


#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param flow           (name) PARAM_DESCRIPTION
#' @param interactive    (logical) PARAM_DESCRIPTION, Default: FALSE
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso 
#'  \code{\link[threejs]{graphjs}}

#'  \code{\link[igraph]{layout_with_sugiyama}}

#'  \code{\link[scales]{alpha}},\code{\link[scales]{hue_pal}}
#' @export 
#' @importFrom threejs graphjs
#' @importFrom igraph layout_with_sugiyama
#' @importFrom scales alpha hue_pal
#' @import htmlwidgets
#' @import threejs
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
    
    plot(flow$graph, 
         edge.arrow.size = .4,
         vertex.label = V(flow$graph)$name, 
         edge.curved = 0, 
         layout = L$layout, 
         rescale = FALSE)
    
  }
  
}


