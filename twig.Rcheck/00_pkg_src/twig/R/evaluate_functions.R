
evaluate_functions <- function(sim, fun_core_df, fun_sim_args, prob_reward_funs, params, arg_value_sizes, fun_args) {

    fun_eval <- list()

    for (fun in prob_reward_funs){

      eval_core_df <- as.list(fun_core_df[[fun]]) 
      eval_sim_args <- stats::setNames(as.list(params[sim, fun_sim_args[[fun]]]), fun_sim_args[[fun]]) 

    tryCatch({
      fun_eval[[fun]] <- do.call(fun, c(eval_core_df, eval_sim_args))
      if (sim == 1){

        if (length(fun_eval[[fun]]) != nrow(fun_core_df[[fun]]) & 
            (length(fun_eval[[fun]]) > 1 | nrow(fun_core_df[[fun]]) > 1)){
          sel_fun_core_sizes <- arg_value_sizes[names(arg_value_sizes) %in% fun_args[[fun]]]
          if (length(ncol(fun_core_df[[fun]])) > 0){
            core_size_string <- paste(paste0(names(sel_fun_core_sizes), "=", sel_fun_core_sizes, collapse = " * "), "\n")
          } else {
            core_size_string <- ""
          }

          stop("Error in function ", fun, ": The function must return a vector of length equal to the product of its core arguments' values.\n",
               "The function returned a vector of length ", length(fun_eval[[fun]]), " but the expected length is ", prod(sel_fun_core_sizes), ".\n",
               core_size_string, "\n", 
               "Rerunning run_twig with verbose = TRUE will generate detailed information on functions data that can be used to debug this error.")
        }
      }
    }, error = function(e) {
      stop("Error in function ", fun, ": ", e$message)
    })
    }
  return(fun_eval)

}
