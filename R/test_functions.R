##%######################################################%##
#                                                          #
####                   Test functions                   ####
#                                                          #
##%######################################################%##

expect_shared_layers <- function(object, other_object, info = NULL) {
  
  # Capture object and label (and expected object and label)
  act <- testthat::quasi_label(rlang::enquo(object))
  exp <- testthat::quasi_label(rlang::enquo(other_object))
  
  # Check that layers are shared by looking at their names
  names1 <- layer_names(act$val)
  names2 <- layer_names(exp$val)
  
  # Rename input, concatenation and flattening layers since they are not shared
  names1 <- gsub(x = names1, pattern = "input.*", replacement = "input")
  names2 <- gsub(x = names2, pattern = "input.*", replacement = "input")
  names1 <- gsub(x = names1, pattern = "concatenate.*", replacement = "concatenate")
  names2 <- gsub(x = names2, pattern = "concatenate.*", replacement = "concatenate")
  names1 <- gsub(x = names1, pattern = "flatten.*", replacement = "flatten")
  names2 <- gsub(x = names2, pattern = "flatten.*", replacement = "flatten")
  
  act$names <- names1
  exp$names <- names2
  comp <- testthat::compare(act$names, exp$names)
  testthat::expect(comp$equal, 
                   sprintf("Some layers are not shared:\n%s not equal to %s.\n%s", 
                           act$lab, 
                           exp$lab, 
                           comp$message), 
                   info = info)
  
  invisible(act$val)
  
}

expect_works <- function(object) testthat::expect_error(object, NA)
