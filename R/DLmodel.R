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
      
      line_to_add <- paste0(timestamp(stamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                      prefix = "[", suffix = "] ", quiet = TRUE),
                            level[1], ": ",
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

      h %>% 
        ggvis(~epochs, ~loss) %>% 
        layer_lines(stroke := "darkgray") %>% 
        layer_smooths(span = 0.2, se = TRUE, fill := "blue", stroke := "darkblue") %>% 
        layer_lines(x = ~epochs,  y = ~val_loss, stroke := "red") %>% 
        add_axis("x", title = "epochs") %>% 
        private$add_title(title = "Losses History")
      
      
    },
    
    render_history = function(initialize = TRUE) {
      
      suppressPackageStartupMessages(require(googleVis))
      
      h <- private$reformat_history()
      
      tmpfile <- tempfile(fileext = ".html")
      foo <- gvisLineChart(h, 
                           xvar = "epochs", 
                           yvar = c("loss", "val_loss"),
                           options = list(title = "Loss History",
                                          titleTextStyle = "{color:'red',fontName:'Courier',fontSize:16}",
                                          curveType = 'function'))
      
      cat(googleVis::renderGvis(foo)(), file = tmpfile)
      
      if (initialize)
        rstudioapi::viewer(url = tmpfile)
      
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
      
    }
    
  ),
  
  private = list(
    
    model = NULL,
    
    width = NULL,
    
    best_loss = Inf,
    
    encoder = NULL,
    decoder = NULL,
    
    hyperparameters = NULL,
    
    log_lines = c(),
    
    history = data.frame(epoch = NULL, subepoch = NULL, time = NULL, loss = NULL, val_loss = NULL),
    
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
        mutate(epochs = (epoch - 1) + (subepoch - 1) / max_sub_epoch) %>% 
        filter(loss >= 0 | val_loss >= 0)
      
      losses <- history2 %>% select(epochs, loss) %>% filter(loss >= 0)
      val_losses <- history2 %>% select(epochs, val_loss) %>% filter(val_loss >= 0)
      h <- inner_join(losses, val_losses)
      
      return(h)
      
    }
    
  )
  
)
