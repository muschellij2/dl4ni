map_images <- function(source, target, nbins = 64) {
  
  source <- source / max(source) * 255
  target <- target / max(target) * 255
  
  h_source <- hist(x = as.vector(source), plot = FALSE, breaks = nbins)
  h_target <- hist(x = as.vector(target), plot = FALSE, breaks = nbins)
  
  counts_source <- h_source$counts / sum(h_source$counts)
  counts_target <- h_target$counts / sum(h_target$counts)
  
  counts_source <- cumsum(counts_source)
  counts_target <- cumsum(counts_target)
  
  require(dtw)
  
  t <- dtw(x = counts_source, y = counts_target, step = symmetric1)
  
  tv <- 1
  sv <- 1

  input_interval <- min(h_source$breaks)
  reference_interval <- min(h_target$breaks)
  
    
  while (tv <= max(t$index2) && sv <= max(t$index1)) {
    
    which_t <- which(t$index2 == tv)
    which_s <- which(t$index1 == sv)
    
    if (length(which_s) > 1) {
      
      t_values <- t$index2[which_s]
      input_interval <- c(input_interval, h_source$breaks[sv + 1])
      reference_interval <- c(reference_interval, h_target$breaks[max(t_values) + 1])
      
      sv <- sv + 1
      tv <- max(t_values) + 1
      
      next
    } 
    
    if (length(which_t) > 1) {
      
      s_values <- t$index1[which_t]
      input_interval <- c(input_interval, h_source$breaks[max(s_values) + 1])
      reference_interval <- c(reference_interval, h_target$breaks[tv + 1])
      
      tv <- tv + 1
      sv <- max(s_values) + 1
      
      next
      
    }
    
    if ((length(which_s) == 1) && (length(which_t) == 1)) {
      
      input_interval <- c(input_interval, h_source$breaks[sv + 1])
      reference_interval <- c(reference_interval, h_target$breaks[tv + 1])
      
      sv <- sv + 1
      tv <- tv + 1
      
    }
    
  }

  new_s <- map_intervals(input = source, 
                         input_intervals = input_interval, 
                         reference_intervals = reference_interval)
  
  return(new_s)
  
}

#' Transform the Graylevels of an Image by Predefined Intervals
#'
#' @param input                   (3D image) Image to be transformed
#' @param input_intervals         (Numeric vector) Ordered values which are considered 
#' graylevel intervals in the input image. See Details.
#' @param reference_intervals     (Numeric vector) Ordered values to which map the input intervals.
#'
#' @details Suppose \code{input_intervals = c(0, 10, 25, 40)} (0 and 40 are the minimum and maximum of the \code{input} image, respectively) and \code{reference_intervals = c(5, 70, 90, 100)}. This transformation linearly maps, consecutively, interval [0, 10] to [5, 70], then [10, 25] to [70, 90], and so.
#'
#' @return The transformed image.
#'
map_intervals <- function(input, input_intervals, reference_intervals, verbose = FALSE) {
  
  # Number of intervals
  n_intervals <- length(input_intervals) - 1
  
  # For each interval, select the gray-levels that lie withtin its extreme values and map their values to 
  # the corresponding reference interval, with a linear interpolation
  res <- 0 * input
  
  for (i in 1:n_intervals) {
    
    input_lo <- input_intervals[i]
    input_hi <- input_intervals[i + 1]
    
    ref_lo <- reference_intervals[i]
    ref_hi <- reference_intervals[i + 1]
    
    if (verbose)
      cat("[", input_lo, ",", input_hi, "] -> [", ref_lo, ",", ref_hi, "]\n")
    
    idx <- which(input >= input_lo & input <= input_hi)
    res[idx] <- (input[idx] - input_lo) / (input_hi - input_lo) * (ref_hi - ref_lo) + ref_lo
    
  }
  
  return(res)
  
}
