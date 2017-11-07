subset_flow <- function(flow, outputs) {
  
  suppressPackageStartupMessages(require(igraph))
  
  # Basic input checks
  stopifnot(inherits(flow, "DLflow"))
  
  # For each required output, get the nodes required for its computation.
  needed_nodes <- c()
  for (out in outputs) {
    
    needed_nodes <- c(needed_nodes, flow$required_inputs[[out]], flow$pipeline[[out]])
    
  }
  
  # Ensure needed nodes are in correct topological order
  needed_nodes <- intersect(igraph::topo_sort(flow$graph), needed_nodes)
  needed_nodes <- flow$outputs[unique(needed_nodes)]
  
  # Create new flow
  new_flow <- new.env()
  new_flow$name <- paste0("subset_", flow$name)
  
  # List of flow inputs and outputs
  new_flow$inputs <- intersect(flow$inputs, needed_nodes)
  new_flow$outputs <- c(new_flow$inputs, setdiff(needed_nodes, new_flow$inputs))
  
  # List of flow processes (both models and functions)
  new_flow$processes <- flow$processes[new_flow$outputs]
  new_flow$trained <- list()
  
  # List of pipelines to execute for each process and of required inputs
  new_flow$pipeline <- list()
  new_flow$required_inputs <- list()
  
  only_output <- setdiff(new_flow$outputs, new_flow$inputs)
  
  for (out in only_output) {
    
    original_inputs <- flow$outputs[flow$required_inputs[[out]]]
    new_inputs <- match(original_inputs, new_flow$inputs)
    new_flow$required_inputs[[out]] <- new_inputs
    
  }
  
  new_flow$inmediate_inputs <- flow$inmediate_inputs[new_flow$outputs]
  
  # Create graph of dependencies
  new_flow$graph <- igraph::make_empty_graph(directed = TRUE)
  
  # Add inputs to the graph
  if (length(new_flow$inputs) > 0) {
    
    new_flow$graph <- new_flow$graph %>% igraph::add_vertices(nv = length(new_flow$inputs), 
                                                      name = unlist(new_flow$inputs), 
                                                      type = rep("Input", length(new_flow$inputs)))
    
  }
  
  # Add nodes to the graph
  types <- V(flow$graph)$type[V(flow$graph)$name %in% only_output]
  new_flow$graph <- new_flow$graph %>% igraph::add_vertices(nv = length(only_output),
                                                            name = only_output,
                                                            type = types)
  
  for (out in only_output) {
    
    node_idx <- which(V(new_flow$graph)$name == out)
    
    inmediate_inputs <- match(new_flow$inmediate_inputs[[out]], new_flow$outputs)
    new_flow$graph <- new_flow$graph %>% igraph::add_edges(edges = as.vector(rbind(inmediate_inputs, node_idx)))
    
  }
  
  # Compute pipeline for each output
  inputs <- which(V(flow$graph)$type == "Input")
  for (target_name in only_output) {
    
    target_idx <- which(V(new_flow$graph)$name == target_name)
    
    # Path from the current node to inputs
    paths <- new_flow$graph %>% all_simple_paths(from = target_idx, to = inputs, mode = "in")
    paths <- lapply(paths, unclass)
    paths <- lapply(paths, as.vector)
    nodes_for_target <- unique(unlist(paths))
    
    # Topological order of the graph
    pipeline <- igraph::topo_sort(new_flow$graph)
    
    # Restricted to nodes connected to the current node
    pipeline <- pipeline[pipeline %in% nodes_for_target]
    
    # Update the list of current required inputs and the pipeline for the current node
    flow$pipeline[[target_name]] <- setdiff(pipeline, inputs)
    
  }
  
  # Create a deep copy of the flow to remove dependencies between kers models
  res_flow <- new_flow %>% clone_flow()
  
  class(res_flow) <- c("DLflow", class(res_flow))
  
  return(invisible(res_flow))
  
}
