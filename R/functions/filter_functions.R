
  # Filter to keep only elements that are functions
  # return an error if function is not found in function names
  filter_functions <- function(fun_names) {
  # Check for the existence of each function
  valid_funs <- fun_names[sapply(fun_names, function(fun_name) {
    exists(fun_name, mode = "function") || fun_name == "leftover"
  })]
  
  # Identify missing functions
  missing_funs <- setdiff(fun_names, valid_funs)
  missing_funs <- setdiff(missing_funs, "leftover")
  
  # Return an error if any functions are missing
  if (length(missing_funs) > 0) {
    stop(paste("The following functions are not found:", paste(missing_funs, collapse = ", ")), call. = FALSE)
  }
  
  return(valid_funs)
}

# filter_functions <- function(fun_names){
  
#     fun_names[sapply(fun_names, function(fun_name) {
#       exists(fun_name, mode = "function")
#     })]
#     #modified_names[modified_names == "cycle_in_state"] <- "expanded_state"
#     #return(modified_names)
#   }


