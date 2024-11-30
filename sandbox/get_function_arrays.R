# nolint start

get_function_arrays <- function(twig_env, n_cycles, n_sims, params) {
    # creating array of function evaluations

    twig_funs <- fun_in_twig(twig_env)
    fun_args <- get_function_arguments(twig_funs)
    fun_args[fun_args == "cycle_in_state"] <- "expanded_state"

    state_lyr <- retrieve_layer_by_type(twig_env, "states")
    # get core arguments
    core_args <- get_core_args(twig_env)

    # get sim arguments
    sim_args <- names(params)

    # used function arguments
    used_core_args <- fun_args[fun_args %in% core_args]

    # used sim arguments
    used_sim_args <- fun_args[fun_args %in% sim_args]

    # everything else need to be either global scalars or numeric scalars.

    # get values of the arguments
    # determine unique function arguments
    fun_arg_values <- list()
    fun_arg_value_sizes <- vector(mode = "integer", length = 0)
    for (arg in used_core_args) {
        fun_arg_values[[arg]] <- get_fun_arg_values(twig_env, arg, n_cycles = n_cycles)
        fun_arg_value_sizes[arg] <- length(fun_arg_values[[arg]])
    }

    # add sim argument if there are parameters
    if (length(used_sim_args) > 0) {
        fun_arg_values[["sim"]] <- 1:n_sims
        fun_arg_value_sizes[["sim"]] <- n_sims
    }

    # get size of argument values - only used core arguments & a single sim argument

    # creating array of function evaluations
    fun_eval_list <- list()
    # and a list of replacement strings for the function arrays
    str_fun_array_list <- list()
    # fun <- twig_funs[4]
    for (fun in twig_funs) {
        # iterate through each function
        fun_args <- get_function_arguments(fun)
        # changing the name of the cycle_in_state argument to expanded_state
        fun_args[fun_args == "cycle_in_state"] <- "expanded_state"
        # if is tunnel expand states, and remove cycle_in_state from the list of arguments
        if ("expanded_state" %in% fun_args) {
            # remove state from the list of arguments
            fun_args <- fun_args[fun_args != "state"]
        }

        # sort arguments by type, and then alphabetically
        core_fun_args <- fun_args[fun_args %in% used_core_args]
        size_core_fun_args <- fun_arg_value_sizes[core_fun_args]
        sim_fun_args <- fun_args[fun_args %in% used_sim_args]
        if (length(sim_fun_args) > 0) {
            size_core_fun_args["sim"] <- n_sims
            expand_dims <- c(core_fun_args, "sim")
        } else {
            expand_dims <- core_fun_args
        }
        other_fun_args <- fun_args[!fun_args %in% used_core_args & !fun_args %in% used_sim_args]
        sorted_fun_args <- c(core_fun_args, sim_fun_args, other_fun_args)
        # get dimension of the function evaluation
        # prod(size_core_fun_args)
        # size_core_fun_args
        dim_prev <- c(1, cumprod(size_core_fun_args))[-(length(size_core_fun_args) + 1)]
        dim_next <- c(rev(cumprod(rev(size_core_fun_args))), 1)[-1]
        names(dim_prev) <- names(dim_next) <- names(size_core_fun_args)
        # iterate through each argument of the function
        idx_list <- list()
        for (arg in expand_dims) {
            idx_list[[arg]] <- rep(1:size_core_fun_args[arg], each = dim_prev[arg], times = dim_next[arg])

            # error trap: make sure the argmenets are either core arguments, in the params list, or are global scalars.
            # error trap: make sure an argument is not in more than one category.
        }

        # iterate through each argument and create a vector or scalar of values to be passed to the function
        list_of_values <- list()
        for (arg in sorted_fun_args) {
            if (arg %in% core_fun_args) {
                # cycle-in-state logic would be different and creates two lists of values
                if (arg == "expanded_state") {
                    # temp_values <- fun_arg_values[[arg]][idx_list[[arg]]]
                    list_of_values[["state"]] <- state_lyr$repeated_states[idx_list[[arg]]]
                    list_of_values[["cycle_in_state"]] <- state_lyr$cycles_in_states[idx_list[[arg]]]
                } else {
                    # need to think about state when there is cycle in state.
                    list_of_values[[arg]] <- fun_arg_values[[arg]][idx_list[[arg]]]
                }
            } else if (arg %in% sim_fun_args) {
                list_of_values[[arg]] <- params[idx_list[["sim"]], arg]
            } else { # has to be global scalar or numeric scalar
                stop(
                    "Argument ", arg, " passed to function", fun,
                    " is not a core twig argument, or a parameter. Core arguments are: ", core_args,
                    ". parameter variables are: ", sim_args, ". scalars need to be defined in the global environment and not passed to the function."
                )
            }
        }
        # evaluate the function

        fun_eval <- do.call(fun, list_of_values)

        # if excel is enabled by default just output the first row of the sim into excel

        dim(fun_eval) <- size_core_fun_args
        dimnames(fun_eval) <- fun_arg_values[expand_dims]
        fun_eval_list[[fun]] <- fun_eval
        str_fun_array_list[[fun]] <- paste0(fun, "[", paste(expand_dims, collapse = ", "), "]")
    }
    twig_env$fun_args <- fun_args
    twig_env$twig_funs <- twig_funs
    twig_env$fun_eval_list <- fun_eval_list
    twig_env$str_fun_array_list <- str_fun_array_list
    twig_env$fun_arg_values <- fun_arg_values
    twig_env$fun_arg_value_sizes <- fun_arg_value_sizes
    #return(twig_env)
}


