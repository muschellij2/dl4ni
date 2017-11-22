instantiate_model <- function(scheme, 
                              problem_info = NULL, 
                              inputs = NULL, 
                              outputs = NULL, 
                              labels_subset = NULL) {
  
  stopifnot(inherits(scheme, "DLscheme"))

  model <- scheme %>% 
    instantiate_model_config(info = problem_info,
                             inputs = inputs,
                             outputs = outputs,
                             labels_subset = labels_subset) %>% 
    create_model_from_config()
  
  return(model)
  
}
