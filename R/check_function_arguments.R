

check_function_arguments <- function(twig_funs, fun_args, core_args, sim_args) {
  core_args <- c(core_args, "cycle_in_state")
  for (fun in twig_funs) {
    sel_fun_args <- fun_args[[fun]]
    extra_args <- sel_fun_args[sel_fun_args %out% c(core_args, sim_args)]
    if (length(extra_args) > 0) {
      stop(
        paste(
          "Error in function ", fun, ":\n",
          "- Invalid argument(s): ", paste(extra_args, collapse = ", "), "\n",
          "- These arguments are not core or simulation arguments.\n",
          "- Additional variables must be defined in the global environment and used directly within the function without passing them as function arguments.\n",
          "- Core arguments: ", paste(core_args, collapse = ", "), "\n",
          "- Simulation arguments: ", paste(sim_args, collapse = ", "), "\n",
          sep = ""
        )
      )
    }
  }
}



check_function_arguments_decision <- function(twig_funs, fun_args, core_args, sim_args) {
  core_args <- c(core_args)
  for (fun in twig_funs) {
    sel_fun_args <- fun_args[[fun]]
    extra_args <- sel_fun_args[sel_fun_args %out% c(core_args, sim_args)]
    if (length(extra_args) > 0) {
      stop(
        paste(
          "Error in function ", fun, ":\n",
          "- Invalid argument(s): ", paste(extra_args, collapse = ", "), "\n",
          "- These arguments are not core or simulation arguments.\n",
          "- Additional variables must be defined in the global environment and used directly within the function without passing them as function arguments.\n",
          "- Core arguments: ", paste(core_args, collapse = ", "), "\n",
          "- Simulation arguments: ", paste(sim_args, collapse = ", "), "\n",
          sep = ""
        )
      )
    }
  }
}
