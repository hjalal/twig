
get_arg_value_sizes <- function(fun_arg_values, core_args, sim_args, n_sims) {
    arg_value_sizes <- vector(mode = "integer", length = 0)
    for (arg in core_args) {
        arg_value_sizes[arg] <- length(fun_arg_values[[arg]])
    }
    if (length(sim_args) > 0) {
        arg_value_sizes[["sim"]] <- n_sims
    }
    return(arg_value_sizes)
}
