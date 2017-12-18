#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param flow       (name) PARAM_DESCRIPTION
#' @param scheme     (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param inputs     (call) PARAM_DESCRIPTION, Default: list()
#' @param output     (name) PARAM_DESCRIPTION
#' @param subset     (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param verbose    (logical) PARAM_DESCRIPTION, Default: TRUE
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso 
#'  
#' @export 
#' @import igraph
add_trainable_model <- function(flow, 
                                scheme = NULL, 
                                inputs,
                                output,
                                subset = NULL,
                                verbose = FALSE) {
  
  suppressPackageStartupMessages(require(igraph))
  
  # Basic checks
  stopifnot(inherits(flow, "DLflow"))
  stopifnot(inherits(scheme, "DLscheme"))
  
  scheme$check()

  # stopifnot(!is.null(scheme$vol_layers_pattern))
  # stopifnot(!is.null(scheme$last_hidden_layers))
  # stopifnot(!is.null(scheme$units) | !is.null(scheme$output_width))
  
  if (is.null(scheme$units))
    scheme$units <- scheme$output_width ^ 3
  
  # Remaining parameters to be computed
  params <- scheme
  params$labels_subset <- subset
  
  class(params) <- c("DLscheme", class(params))
  
  # Store the model in the flow
  if (verbose)
    cat("Storing configuration\n")
  
  flow %>% add_process(proc = params, inputs = inputs, output = output, trained = FALSE)
  
  return(invisible(flow))
  
}
