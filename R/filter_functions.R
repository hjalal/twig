
  
  
  filter_functions <- function(fun_names, is_prob_reward = TRUE) {
  
  valid_funs <- fun_names[sapply(fun_names, function(fun_name) {
    exists(fun_name, mode = "function") 
  })]
  
  
  missing_funs <- setdiff(fun_names, valid_funs)
  missing_funs <- setdiff(missing_funs, "leftover")
  
  
  if (length(missing_funs) > 0 & is_prob_reward) {
    stop(paste("The following function(s) are used in the twig but are not defined:", paste(missing_funs, collapse = ", ")), call. = FALSE)
  }
  
  return(valid_funs)
}


  








