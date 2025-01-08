evaluate_p0_functions <- function(fun_core_df, fun_sim_args, p0_funs, params) {
    fun_eval_p0 <- list()
    
    for (fun in p0_funs){
      
      eval_core_df <- as.list(fun_core_df[[fun]]) 
      eval_sim_args <- as.list(params[, fun_sim_args[[fun]], drop = FALSE])
      
      
    tryCatch({
      fun_eval_p0[[fun]] <- do.call(fun, c(eval_core_df, eval_sim_args))
    }, error = function(e) {
      stop("Error in function ", fun, ": ", e$message, ". Make sure the function executes correctly and that all variables used in the function are defined.")
    })
    }
  return(fun_eval_p0)

}
