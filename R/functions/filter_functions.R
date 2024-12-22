
  # Filter to keep only elements that are functions
  filter_functions <- function(fun_names){
    fun_names[sapply(fun_names, function(fun_name) {
      exists(fun_name, mode = "function")
    })]
    #modified_names[modified_names == "cycle_in_state"] <- "expanded_state"
    #return(modified_names)
  }
