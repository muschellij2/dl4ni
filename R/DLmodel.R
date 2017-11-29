#' DLmodel Class
#'
#' @docType class
#' @importFrom R6 R6Class
#'
#' @export
#' @keywords data
#'
#' @return Object of class \code{\link{R6Class}} and \code{DLmodel}.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @examples
#' DLmodel$new()
#'
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method follow the corresponding link. }
#'   \item{\code{initialize(...)}}{Create new object. Documented in \link{DLmodel.initialize}.}
#'   \item{\code{update(...)}}{Update fields of the object. Documented in \link{DLmodel.update}.}
#'   \item{\code{summary()}}{Model summary. Documented in \link{DLmodel.summary}.}
#'   \item{\code{log(level = c("DEBUG", "INFO", "WARNING", "ERROR"), message = '...')}}{Add a message to the object log. Documented in \link{DLmodel.log}.}
#'   \item{\code{add_to_history(epoch = 0, subepoch = 0, time = Sys.time(), loss = NA, val_loss = NA)}}{Adds a new entry to the loss history of the model. Documented in \link{DLmodel.add_to_history}.}
#'   \item{\code{plot_history()}}{Plots this object's loss history. Documented in \link{DLmodel.plot_history}.}
#'   \item{\code{reset_history()}}{Deletes all loss history of this object. Documented in \link{DLmodel.reset_history}.}
#'   \item{\code{render_history(initialize = TRUE)}}{Uses RStudio Viewer pane to plot loss history. Documented in \link{DLmodel.render_history}.}
#'   \item{\code{update_render()}}{Updates the render of loss history in Viewer pane. Documented in \link{DLmodel.update_render}.}
#'   \item{\code{get_model()}}{Returns the keras model. Documented in \link{DLmodel.get_model}.}
#'   \item{\code{get_width()}}{Window width used to train the model. Documented in \link{DLmodel.get_width}.}
#'   \item{\code{set_width(width)}}{Sets window width for the model. Documented in \link{DLmodel.set_width}.}
#'   \item{\code{get_loss()}}{Returns the best loss achieved by the model, or \code{Inf} if it hasn't been trained yet. Documented in \link{DLmodel.get_loss}.}
#'   \item{\code{set_loss(loss)}}{Sets the loss of the model. Documented in \link{DLmodel.set_loss}.}
#'   \item{\code{get_encoder()}}{If the model is an autoencoder, returns the encoder part. Documented in \link{DLmodel.get_encoder}.}
#'   \item{\code{get_decoder()}}{If the model is an autoencoder, returns the decoder part. Documented in \link{DLmodel.get_decoder}.}
#'   \item{\code{get_config()}}{Returns the configuration of the model. From it, one can re-build the whole model. Documented in \link{DLmodel.get_config}.}
#'   \item{\code{get_history()}}{Returns the loss history as a data.frame. Documented in \link{DLmodel.get_history}.}
#'   \item{\code{print_log(level = c("DEBUG", "WARNING", "INFO", "ERROR"))}}{Prints this object's log. Documented in \link{DLmodel.print_log}.}
#'   \item{\code{errors()}}{Prints errors produced when using this object. Documented in \link{DLmodel.errors}.}
#'   \item{\code{warnings()}}{Prints warnings produced when using this object. Documented in \link{DLmodel.warnings}.}
#'   \item{\code{save_log(filename, level = c("DEBUG", "WARNING", "INFO", "ERROR"))}}{Saves log to a file. Documented in \link{DLmodel.save_log}.}
#'   \item{\code{check_memory()}}{Checks whether the model can be trained given current memory limits. Documented in \link{DLmodel.check_memory}.}
#'   \item{\code{graph()}}{Returns the graph of the model. Documented in \link{DLmodel.graph}.}
#'   \item{\code{plot(to_file)}}{Plots this model's graph. Documented in \link{DLmodel.plot}.}
#'   \item{\code{load(path, prefix)}}{Load a model stored in a given path, given file prefix. Documented in \link{DLmodel.load}.}
#'   \item{\code{save(path, prefix, comment)}}{Saves the model in a given path, given file prefix. Documented in \link{DLmodel.save}.}
#'   \item{\code{use_data(use = c("train", "test"), x_files, y_files = NULL, target_windows_per_file = 1024)}}{Assigns data for training r testing, to be used when fitting the model. Documented in \link{DLmodel.use_data}.}
#'   \item{\code{fit(epochs = 10, keep_best = TRUE, metrics_viewer = FALSE, ...)}}{Trains the model. Documented in \link{DLmodel.fit}.}
#'   \item{\code{reset()}}{Resets the model to the original (untrained) state. Documented in \link{DLmodel.reset}.}
#'   \item{\code{infer(V = NULL, speed = c("faster", "medium", "slower"))}}{Run inference over a volume. Documented in \link{DLmodel.infer}.}
#'   \item{\code{clone(deep = FALSE)}}{Clones the model. Documented in \link{DLmodel.clone}.}
#'  }
DLmodel <- R6::R6Class(
  
  classname = "DLmodel",
  
  public = list(
    
    initialize = function(...) {
      
      load_keras()
      suppressPackageStartupMessages(require(tidyverse))
      suppressPackageStartupMessages(require(ggvis))
      
      args <- list(...)
      
      if (!is.null(args$model)) {
        
        private$model <- args$model
        
      }
      
      if (!is.null(args$width)) {
        
        private$width <- args$width
        
      }
      
      if (!is.null(args$best_loss)) {
        
        private$best_loss <- args$best_loss
        
      }
      
      if (!is.null(args$encoder)) {
        
        private$encoder <- args$encoder
        
      }
      
      if (!is.null(args$decoder)) {
        
        private$decoder <- args$decoder
        
      }
      
      if (!is.null(args$hyperparameters)) {
        
        private$hyperparameters <- args$hyperparameters
        
      }
      
    },
    
    update = function(...) {
      
      self$initialize(...)
      
    },
    
    summary = function() {
      
      summary(private$model)
      
    },
    
    log = function(level = c("DEBUG", "INFO", "WARNING", "ERROR"), message = "...") {
      
      line_to_add <- paste0("(", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ") [",
                            level[1], "] ",
                            message)
      
      private$log_lines <- c(private$log_lines, line_to_add)
      
    },
    
    add_to_history = function(epoch = 0, 
                              subepoch = 0, 
                              time = Sys.time(), 
                              loss = NA, 
                              val_loss = NA) {
      
      private$history <- rbind(private$history,
                               data.frame(epoch = epoch,
                                          subepoch = subepoch,
                                          time = time,
                                          loss = loss,
                                          val_loss = val_loss))
      
    },
    
    plot_history = function() {
      
      require(ggvis)
      
      h <- private$reformat_history()
      
      span <- ifelse(nrow(h) < 25, 0.5, 0.2)
      
      if (length(h$val_loss[h$val_loss >= 0]) == 1) {
        
        h %>% 
          ggvis(~epochs, ~loss) %>% 
          layer_lines(stroke := "darkgray") %>% 
          layer_smooths(span = span, se = TRUE, fill := "blue", stroke := "darkblue") %>% 
          layer_points(x = ~epochs[val_loss >= 0],  y = ~val_loss[val_loss >= 0], fill := "red", stroke := "darkred") %>% 
          add_axis("x", title = "epochs") %>% 
          add_axis("y", title = "loss") %>% 
          private$add_title(title = "Losses History")
        
      } else {
        
        h %>% 
          ggvis(~epochs, ~loss) %>% 
          layer_lines(stroke := "darkgray") %>% 
          layer_smooths(span = span, se = TRUE, fill := "blue", stroke := "darkblue") %>% 
          layer_lines(x = ~epochs[val_loss >= 0],  y = ~val_loss[val_loss >= 0], stroke := "darkred") %>% 
          add_axis("x", title = "epochs") %>% 
          add_axis("y", title = "loss") %>% 
          private$add_title(title = "Losses History")
        
      }
      
    },
    
    reset_history = function() {
      
      private$history <- data.frame(epoch = NULL, subepoch = NULL, time = NULL, loss = NULL, val_loss = NULL)
      
    },
    
    render_history = function(initialize = TRUE) {
      
      suppressPackageStartupMessages(require(googleVis) & require(rstudioapi))
      
      h <- private$reformat_history()
      
      foo <- gvisLineChart(h, 
                           xvar = "epochs", 
                           yvar = c("loss", "val_loss"),
                           options = list(width = 480,
                                          height = 640,
                                          title = "Loss History",
                                          titleTextStyle = "{color:'red',fontSize:16}"))
      
      cat(renderGvis(foo)(), file = private$render_file)
      
      if (initialize)
        viewer(url = private$render_file)
      
    },
    
    update_render = function() {
      
      self$render_history(initialize = FALSE)
      
    },
    
    get_model = function() {
      
      return(private$model)
      
    },
    
    get_width = function() {
      
      return(private$width)
      
    },
    
    set_width = function(width) {
      
      private$width <- width
      
    },
    
    get_loss = function() {
      
      return(private$best_loss)
      
    },
    
    set_loss = function(loss) {
      
      private$best_loss <- loss
      
    },
    
    get_encoder = function() {
      
      return(private$encoder)
      
    },
    
    get_decoder = function() {
      
      return(private$decoder)
      
    },
    
    get_config = function() {
      
      return(private$hyperparameters)
      
    },
    
    get_history = function() {
      
      return(private$history)
      
    },
    
    print_log = function(level = c("DEBUG", "WARNING", "INFO", "ERROR")) {
      
      lines <- private$get_log_lines(level)
      
      cat(lines, sep = "\n")
      
    },
    
    errors = function() {
      
      self$print_log(level = "ERROR")
      
    },
    
    warnings = function() {
      
      self$print_log(level = "WARNING")
      
    },
    
    save_log = function(filename, level = c("DEBUG", "WARNING", "INFO", "ERROR")) {
      
      lines <- private$get_log_lines(level)
      
      cat(lines, sep = "\n", file = filename)
      
    },
    
    check_memory = function() {
      
      batch_size <- self %>% compute_batch_size()
      
      # If batch_size == 0, there is no possibility of training with the specified memory limit.
      if (batch_size < 1) {
        
        required_memory <- prettyunits::pretty_bytes(unclass(self %>% model_size() * 4))
        
        # Not enough memory to train even 1 batch at a time
        error_message <- paste0("Not enough memory to train this model. Optimal batch size is 0 for the memory limit: ", 
                                prettyunits::pretty_bytes(private$hyperparameters$memory_limit), "\n",
                                "This model requires at least ", required_memory, " to be trained.\n",
                                "We suggest to increase this limit by adding: memory_limit = ", required_memory, " to the scheme.\n")
        
        self$log("ERROR", message = error_message)
        
        stop(error_message)
        
      } else {
        
        return(invisible(batch_size))
        
      }
      
    },
    
    graph = function() {
      
      self %>% graph_from_model()
      
    },
    
    plot = function(to_file) {
      
      self %>% plot_model(to_file = to_file)
      
    },
    
    load = function(path, prefix) {
      
      self <- load_model(path = path, prefix = prefix)
      
    },
    
    save = function(path, prefix, comment = "") {
      
      self %>% save_model(path, prefix, comment)
      
    },
    
    use_data = function(use = c("train", "test"),
                        x_files, 
                        y_files = NULL,
                        target_windows_per_file = 1024) {
      
      gen <- self %>% create_generator(x_files = x_files, 
                                       y_files = y_files,
                                       mode = "sampling",
                                       target_windows_per_file = target_windows_per_file)
      
      if (use[1] == "train") {
        
        private$train_config <- gen
        private$train_files <- list(x = x_files, y = y_files)
        self$has_train_data <- TRUE
        
      } else {
        
        private$test_config <- gen
        private$test_files <- list(x = x_files, y = y_files)
        
      }
      
    },
    
    fit = function(epochs = 10, 
                   keep_best = TRUE,
                   metrics_viewer = FALSE,
                   ...) {
      
      args <- list(...)
      
      if (keep_best) {
        
        # Path and prefix must be provided
        
        path <- args$path
        prefix <- args$prefix
        
        if (is.null(path) | is.null(prefix)) {
          
          self$log("ERROR", message = "No path and prefix provided when training.")
          
          message <- "When training with keep_best == TRUE, path and prefix must be provided."
          stop(message)
          
        }
        
      }
      
      if (!self$has_train_data) {
        
        self$log("ERROR", message = "No training configuration provided.")
        stop("No training configuration provided. Employ 'use_data' function to provide a training configuration.")
        
      }
      
      self %>% fit_with_generator(train_config = private$train_config,
                                  validation_config = private$test_config,
                                  epochs = epochs,
                                  starting_epoch = private$last_epoch + 1,
                                  keep_best = keep_best,
                                  path = path,
                                  prefix = prefix,
                                  metrics_viewer = metrics_viewer)
      
      private$last_epoch <- max(private$history$epoch)
      
    },
    
    reset = function() {
      
      self %>% reset_model()
      
    },
    
    infer = function(V = NULL, 
                     speed = c("faster", "medium", "slower")) {
      
      self %>% infer_on_volume(V = V, speed = speed[1])
      
    },
    
    has_train_data = FALSE
    
  ),
  
  private = list(
    
    model = NULL,
    train_config = NULL,
    test_config = NULL,
    
    train_files = NULL,
    test_files = NULL,
    
    last_epoch = 0,
    
    width = NULL,
    
    best_loss = Inf,
    
    encoder = NULL,
    decoder = NULL,
    
    hyperparameters = NULL,
    
    log_lines = c(),
    
    history = data.frame(epoch = NULL, subepoch = NULL, time = NULL, loss = NULL, val_loss = NULL),
    
    render_file = tempfile(fileext = ".html"),
    
    get_log_lines = function(level = c("DEBUG", "WARNING", "INFO", "ERROR")) {
      
      lines <- c()
      
      if ("DEBUG" %in% level)
        level <- c("DEBUG", "INFO", "WARNING", "ERROR")
      
      for (i in seq_along(level)) {
        
        lines <- c(lines, grep(private$log_lines, pattern = level[i]))
        
      }
      
      lines <- sort(lines)
      
      lines <- private$log_lines[lines]
      
      return(lines)
      
    },
    
    # ggvis lacks a plot title function, so add one.
    # based on clever hack by tonytonov
    # http://stackoverflow.com/a/25030002/1135316
    add_title = function(vis, ..., properties = NULL, title = "Plot Title") {
      
      # recursively merge lists by name
      # http://stackoverflow.com/a/13811666/1135316
      merge.lists <- function(a, b) {
        a.names <- names(a)
        b.names <- names(b)
        m.names <- sort(unique(c(a.names, b.names)))
        sapply(m.names, function(i) {
          if (is.list(a[[i]]) & is.list(b[[i]])) merge.lists(a[[i]], b[[i]])
          else if (i %in% b.names) b[[i]]
          else a[[i]]
        }, simplify = FALSE)
      }
      
      # default properties make title 'axis' invisible
      default.props <- axis_props(
        ticks = list(strokeWidth = 0),
        axis = list(strokeWidth = 0),
        labels = list(fontSize = 0),
        grid = list(strokeWidth = 0)
      )
      
      # merge the default properties with user-supplied props.
      axis.props <- do.call(axis_props, merge.lists(default.props, properties))
      
      # don't step on existing scales.
      vis <- scale_numeric(vis, "title", domain = c(0,1), range = 'width')
      axis <- ggvis:::create_axis('x', 'title', orient = "top",  title = title, properties = axis.props, ...)
      ggvis:::append_ggvis(vis, "axes", axis)
      
    },
    
    reformat_history = function() {
      
      suppressPackageStartupMessages(require(tidyverse))
      
      max_sub_epoch <- max(private$history %>% select(subepoch) %>% unlist())
      
      history2 <- private$history %>% 
        mutate(epochs = (epoch - 1) + (subepoch - 1) / max_sub_epoch)
      
      losses <- history2 %>% select(epochs, loss) %>% filter(loss >= 0)
      val_losses <- history2 %>% select(epochs, val_loss) %>% filter(val_loss >= 0)
      h <- full_join(losses, val_losses, by = "epochs")
      if (nrow(h) >= 2)
        if (all(h[1, ] == h[2, ]))
          h <- h[-1, ]
      
      return(h)
      
    }
    
  )
  
)
