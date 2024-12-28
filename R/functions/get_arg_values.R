get_arg_values <- function(twig_obj, used_core_args, sim_args, n_cycles = NULL) {
    fun_arg_values <- list()
    for (arg in used_core_args) {
        fun_arg_values[[arg]] <- get_fun_arg_values(twig_obj, arg, n_cycles = n_cycles)
    }
    if (length(sim_args) > 0) {
        fun_arg_values[["sim"]] <- 1:n_sims
    }
    return(fun_arg_values)
}
