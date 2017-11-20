plot_model <- function(object, to_file = "model.png") {
  
  stopifnot(inherits(object, "DLmodel"))
  model <- object$get_model()
  
  keras:::keras$utils$vis_utils$plot_model(model, to_file = to_file, show_shapes = TRUE)
  
}
