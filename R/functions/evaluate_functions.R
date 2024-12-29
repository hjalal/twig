# evaluate functions given a simulation and their design of inputs

evaluate_functions <- function(sim, fun_core_df, fun_sim_args, prob_reward_funs, params) {
    fun_eval <- list()
    #fun <- prob_reward_funs[4]
    for (fun in prob_reward_funs){
      
      eval_core_df <- as.list(fun_core_df[[fun]]) 
      eval_sim_args <- setNames(as.list(params[sim, fun_sim_args[[fun]]]), fun_sim_args[[fun]]) #as.list(params[sim, fun_sim_args[[fun]]])
      # Evaluate the function for each permutation
      #do.call(fun, as.list(permutations))
    tryCatch({
      fun_eval[[fun]] <- do.call(fun, c(eval_core_df, eval_sim_args))
    }, error = function(e) {
      stop("Error in function ", fun, ": ", e$message, ". Make sure the function executes correctly and that all variables used in the function are defined.")
    })
    }
  return(fun_eval)

}
