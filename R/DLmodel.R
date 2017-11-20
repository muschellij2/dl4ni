DLmodel <- R6::R6Class(
  
  classname = "DLmodel",
  
  public = list(
    
    initialize = function(...) {
      
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
      
      private$history <- c(private$history,
                           data.frame(epoch = epoch,
                                      subepoch = subepoch,
                                      time = time,
                                      loss = loss,
                                      val_loss = val_loss))
      
    },
    
    plot_history = function() {
      
      require(ggplot2)
      
      qplot(data = private$history, aes(x = epoch, y = loss))
      
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
    
    print_log = function(level = c("DEBUG", "WARNING", "INFO", "ERROR")) {
      
      cat(private$log_lines, sep = "\n")
      
    },
    
    save_log = function(filename, level = c("DEBUG", "WARNING", "INFO", "ERROR")) {
      
      cat(private$log_lines, sep = "\n", file = filename)
      
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
    
    history = data.frame(epoch = NULL, subepoch = NULL, time = NULL, loss = NULL, val_loss = NULL)
    
  )
  
)
