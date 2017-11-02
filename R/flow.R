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

add_inputs <- function(flow, inputs = list()) {
  
  # Basic checks
  stopifnot(inherits(flow, "DLflow"))
  
  # Add inputs to the graph
  if (length(inputs) > 0) {
    flow$inputs <- c(flow$inputs, inputs)
    flow$graph <- flow$graph %>% igraph::add_vertices(nv = length(inputs), 
                                                      name = unlist(inputs), 
                                                      type = rep("Input", length(inputs)))
    
  }
  
  # List of all possible outputs of the flow
  flow$outputs <- c(flow$outputs, unlist(inputs))
  
  return(invisible(flow))
  
}

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param flow      (name) PARAM_DESCRIPTION
#' @param proc      (name) PARAM_DESCRIPTION
#' @param inputs    (call) PARAM_DESCRIPTION, Default: list()
#' @param output    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso 
#'  \code{\link[igraph]{add_vertices}},\code{\link[igraph]{add_edges}}
#' @export 
#' @importFrom igraph add_vertices add_edges
add_process <- function(flow, 
                        proc, 
                        inputs = ifelse(inherits(proc, "function"), list(names(formals(proc))), list()),
                        output) {
  
  # Basic checks
  stopifnot(inherits(flow, "DLflow"))
  stopifnot(inherits(proc, "DLmodel") | inherits(proc, "function"))
  output <- as.character(output)
  
  if (length(inputs) > 0) inputs <- unlist(inputs)
  
  type <- "DLmodel"
  if (inherits(proc, "function")) type <- "function"
  
  # Add a node to the graph, with edges from its inputs to it.
  flow$graph <- flow$graph %>% igraph::add_vertices(nv = 1, name = output, type = type)
  new_vertex_idx <- length(V(flow$graph))
  
  if (length(inputs) > 0) {
    
    input_ids <- match(inputs, flow$outputs)
    flow$inmediate_inputs[[output]] <- flow$outputs[input_ids]
    
    flow$graph <- flow$graph %>% igraph::add_edges(edges = as.vector(rbind(input_ids, new_vertex_idx)))
    
  }
  
  # Add the model to the list of flow models
  flow$processes[[output]] <- proc
  flow$outputs <- c(flow$outputs, output)
  
  # Add its pipeline (updating all previous pipelines)
  inputs <- which(V(flow$graph)$type == "Input")
  for (target_idx in setdiff(seq(new_vertex_idx), inputs)) {
    
    # Path from the current node to inputs
    paths <- flow$graph %>% all_simple_paths(from = target_idx, to = inputs, mode = "in")
    paths <- lapply(paths, unclass)
    paths <- lapply(paths, as.vector)
    nodes_for_target <- unique(unlist(paths))
    
    # Topological order of the graph
    pipeline <- topo_sort(flow$graph)
    
    # Restricted to nodes connected to the current node
    pipeline <- pipeline[pipeline %in% nodes_for_target]
    
    # Update the list of current required inputs and the pipeline for the current node
    flow$required_inputs[[V(flow$graph)$name[target_idx]]] <- intersect(pipeline, inputs)
    flow$pipeline[[V(flow$graph)$name[target_idx]]] <- setdiff(pipeline, inputs)
    
  }
  
  return(invisible(flow))
  
}

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

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param flow               (name) PARAM_DESCRIPTION
#' @param inputs             (call) PARAM_DESCRIPTION, Default: list()
#' @param desired_outputs    (NULL) PARAM_DESCRIPTION, Default: NULL
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
execute_flow <- function(flow, inputs = list(), desired_outputs = NULL) {
  
  stopifnot(inherits(flow, "DLflow"))
  
  # Check that inputs is a named list of files and that all of them exist
  all_exist <- all(sapply(inputs, file.exists))
  
  if (!all_exist)
    stop("Not all input files exist.")
  
  input_names <- names(inputs)
  input_names <- input_names[input_names %in% flow$inputs]
  
  # Initialize computed_outputs
  flow$computed_outputs <- list()
  
  # Check that the desired outputs can be computed
  all_computable <- all(desired_outputs %in% flow$outputs)
  if (!all_computable)
    warning("Some of the outputs cannot be computed.")
  
  desired_outputs <- desired_outputs[desired_outputs %in% flow$outputs]
  
  results <- list()
  
  if (length(desired_outputs) > 0) {
    
    # Read inputs
    for (name in input_names) {
      
      flow$computed_outputs[[name]] <- neurobase::readnii(inputs[[name]])
      
    }
    
    # For each output
    for (output in desired_outputs) {
      
      # Are all the required inputs?
      inputs_idx <- unique(flow$required_inputs[[output]])
      all_required <- all(V(flow$graph)$name[inputs_idx] %in% input_names)
      
      if (!all_required) {
        
        message <- paste0("Not all required inputs for ", output)
        warning(message)
        
        next
        
      }
      
      # Define which parts of the flow must be processed
      pipeline <- flow$pipeline[[output]]
      
      # Execute in order
      for (process_idx in pipeline) {
        
        intermediate_output <- flow$outputs[process_idx]
        
        # if this process is already computed, go to the next one
        if (!is.null(flow$computed_outputs[[intermediate_output]])) next
        
        cat("Computing", intermediate_output, "...\n")
        
        process <- flow$processes[[intermediate_output]]
        my_inputs <- flow$inmediate_inputs[[intermediate_output]]
        
        switch(V(flow$graph)$type[process_idx],
               
               "function" = {
                 
                 params <- flow$computed_outputs[my_inputs]
                 names(params) <- names(formals(process))
                 flow$computed_outputs[[intermediate_output]] <- do.call(what = process, args = params)
                 
               },
               
               "DLmodel" = {
                 
                 # Inference function for the given model
                 infer <- process$hyperparameters %>% create_inference_function_from_config()
                 
                 # Infer on input volumes
                 input_imgs <- flow$computed_outputs[my_inputs]
                 flow$computed_outputs[[intermediate_output]] <- process %>% infer(V = input_imgs, spped = "medium")
                 
               })
        
      }
      
      
    }
    
    results <- flow$computed_outputs[desired_outputs]
    
  }
  
  
  return(results)
  
}

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param flow    (name) PARAM_DESCRIPTION
#' @param path    (call) PARAM_DESCRIPTION, Default: tempdir()
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
save_flow <- function(flow, path = tempdir()) {
  
  # Models must be saved apart.
  
}

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param filename    (name) PARAM_DESCRIPTION
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
load_flow <- function(filename) {
  
  
  
}

#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param flow              (name) PARAM_DESCRIPTION
#' @param epochs            (numeric) PARAM_DESCRIPTION, Default: 10
#' @param max_sub_epochs    (numeric) PARAM_DESCRIPTION, Default: 5
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @export 
train_flow <- function(flow, epochs = 10, max_sub_epochs = 5) {
  
  stopifnot(inherits(flow, "DLflow"))
  
  
}
