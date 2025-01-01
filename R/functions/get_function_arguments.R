
# takes out function arguments from the function name definition.
get_function_args <- function(fun_name) {

    # For multiple functions, get unique arguments across all
    arguments <- lapply(fun_name, function(fname) {
      fun <- get(fname)
      names(formals(fun))

      # if there is cycle_in_state, replace it with expanded_state, and remove state
      # if ("cycle_in_state" %in% fun_args) {
      #   fun_args[fun_args == "cycle_in_state"] <- "expanded_state"
      #   fun_args <- fun_args[fun_args != "state"]
      # }
      #fun_args
    })
    names(arguments) <- fun_name

  return(arguments)
}