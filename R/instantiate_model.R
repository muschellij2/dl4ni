instantiate_model <- function(scheme, problem_info) {
  
  stopifnot(inherits(scheme, "DLscheme"))
  stopifnot(inherits(problem_info, "DLproblem"))
  
  model <- scheme %>% 
    instantiate_model_config(problem_info) %>% 
    create_model_from_config()
  
  return(model)
  
}
