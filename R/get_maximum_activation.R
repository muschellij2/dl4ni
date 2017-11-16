get_maximum_activation <- function(.model, 
                                   layer_weights = list(), 
                                   filters = NULL, 
                                   continuity = 0.1, 
                                   dream_l2 = 0.02, 
                                   jitter = 0.1, 
                                   plot = TRUE) {
  
  library(tensorflow)
  library(reticulate)
  
  
  K <- keras::backend()
  K$set_learning_phase(FALSE)
  
  # From reticulate.
  array_reshape <- function(x, dim, order = c("C", "F")) {
    np <- import("numpy", convert = FALSE)
    order <- match.arg(order)
    reshaped <- np$reshape(x, as.integer(dim), order)
    if (!inherits(x, "python.builtin.object"))
      reshaped <- py_to_r(reshaped)
    reshaped
  }
  
  # Calculates the total variation loss
  # See https://en.wikipedia.org/wiki/Total_variation_denoising 
  # for further explanation
  total_variation_loss <- function(x, h, w, d){
    
    x <- K$reshape(x, shape = c(h, w, d))
    
    h <- h - 1L
    w <- w - 1L
    d <- d - 1L
    
    y_ijk  <- x[1:(h - 1L), 1:(w - 1L), 1:(d - 1L)]
    y_i1jk <- x[2:(h), 1:(w - 1L), 1:(d - 1L)]
    y_ij1k <- x[1:(h - 1L), 2:(w), 1:(d - 1L)]
    y_ijk1 <- x[1:(h - 1L), 1:(w - 1L), 2:(d)]
    
    a <- K$square(y_ijk - y_i1jk)
    b <- K$square(y_ijk - y_ij1k)
    c <- K$square(y_ijk - y_ijk1)
    K$sum(K$pow(a + b + c, 1.25))
    
  }
  
  normalize <- function(x) {
    
    x / (K$sqrt(K$mean(K$square(x))) + 1e-5)
    
  }
  
  
  stopifnot(inherits(.model, "DLmodel"))
  
  .model %>% set_trainability(trainability = FALSE)
  
  model <- .model$model
  config <- .model$hyperparameters
  
  # The settings to be used in this experiment
  img_size <- c(config$width, config$width, config$width)
  settings <- list(features = layer_weights,
                   continuity = continuity,
                   dream_l2 = dream_l2,
                   jitter = jitter)
  
  
  dream <- model$layers[[1]]$input
  
  # Get the symbolic outputs of each "key" layer (we gave them unique names).
  layer_dict <- model$layers
  names(layer_dict) <- purrr::map_chr(layer_dict, ~.x$name)
  
  # Define the loss
  loss <- tensorflow::tf$Variable(0.0)
  
  for (layer_name in names(settings$features)) {
    
    # Add the L2 norm of the features of a layer to the loss
    coeff <- settings$features[[layer_name]]
    x <- layer_dict[[layer_name]]$output
    out_shape <- layer_dict[[layer_name]]$output_shape %>% unlist()
    
    if (is.null(filters)) {
      
      # Avoid border artifacts by only involving non-border pixels in the loss
      loss <- loss - 
        coeff * K$sum(K$square(x[ , , , , ])) / 
        prod(out_shape)
      
    } else {
      
      for (filter in filters) {
        
        # Avoid border artifacts by only involving non-border pixels in the loss
        loss <- loss - 
          coeff * K$sum(K$square(x[ , , , , as.integer(filter)])) / 
          prod(out_shape) * length(filters)
        
      }
      
    }
    
    
  }
  
  # Add continuity loss (gives image local coherence, can result in an artful blur)
  loss <- loss + settings$continuity *
    total_variation_loss(x = dream, as.integer(config$width), as.integer(config$width), as.integer(config$width)) / prod(img_size)
  
  # Add image L2 norm to loss (prevents pixels from taking very high values, makes image darker)
  # Note that the loss can be further modified to achieve new effects
  loss <- loss + settings$dream_l2 * K$sum(K$square(dream)) / prod(img_size)
  
  # Compute the gradients of the dream wrt the loss
  grads <- K$gradients(loss, dream)[[1]]
  
  grads <- normalize(grads)
  
  f_outputs <- K$`function`(list(dream), list(loss, grads))
  
  eval_loss_and_grads <- function(image){
    image <- array_reshape(image, c(1, prod(img_size)))
    outs <- f_outputs(list(image))
    list(
      loss_value = outs[[1]],
      grad_values = array_reshape(outs[[2]], dim = length(outs[[2]]))
    )
  }
  
  # Loss and gradients evaluator:
  # This Evaluator class makes it possible to compute loss and 
  # gradients in one pass while retrieving them via two separate 
  # functions, "loss" and "grads". This is done because 
  # scipy.optimize requires separate functions for loss and 
  # gradients, but computing them separately would be inefficient.
  Evaluator <- R6::R6Class(
    "Evaluator",
    public = list(
      
      loss_value = NULL,
      grad_values = NULL,
      
      initialize = function() {
        self$loss_value <- NULL
        self$grad_values <- NULL
      },
      
      loss = function(x){
        loss_and_grad <- eval_loss_and_grads(x)
        self$loss_value <- loss_and_grad$loss_value
        self$grad_values <- loss_and_grad$grad_values
        self$loss_value
      },
      
      grads = function(x){
        grad_values <- self$grad_values
        self$loss_value <- NULL
        self$grad_values <- NULL
        grad_values
      }      
    )
  )
  
  evaluator <- Evaluator$new()
  
  
  input_img_data <- runif(prod(img_size)) * 1.e-5
  image <- input_img_data
  # input_img_data <- scale_z(input_img_data)
  
  # Run optimization (L-BFGS) over the pixels of the generated image
  # so as to minimize the loss
  # Add random jitter to initial image
  random_jitter <- settings$jitter * 2 * (runif(prod(img_size)) - 0.5) %>%
    array(dim = c(prod(img_size)))
  image <- image + random_jitter
  
  # Run L-BFGS
  opt <- optim(
    array_reshape(image, dim = length(image)), fn = evaluator$loss, gr = evaluator$grads,
    method = "L-BFGS-B",
    control = list(maxit = 2)
  )
  
  #Print loss value
  cat("Loss:", opt$value, "\n")
  
  # Decode the image
  image <- opt$par - random_jitter
  
  # Plot
  if (plot)
    neurobase::ortho2(array_reshape(image, c(img_size)), text = paste0("Maximum Activation"))
  
  return(array_reshape(image, c(img_size)))
  
}
