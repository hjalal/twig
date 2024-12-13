# nolint start

source("sandbox/functions/fun_in_twig.R")

evaluate_prob_reward_functions <- function(twig_obj, n_cycles, n_sims, params) {
 
    list_evaluated_funs <- list()
    str_fun_array_list <- list()
    fun_args_expanded <- list()

    for (fun in twig_funs) {
        fun_args <- get_function_arguments(fun)
        fun_args <- update_fun_args(fun_args)

        core_fun_args <- fun_args[fun_args %in% used_core_args]
        size_core_fun_args <- fun_arg_value_sizes[core_fun_args]
        sim_fun_args <- fun_args[fun_args %in% used_sim_args]
        expand_dims <- get_expand_dims(core_fun_args, sim_fun_args)
        fun_args_expanded[[fun]] <- expand_dims
        sorted_fun_args <- get_sorted_fun_args(fun_args, core_fun_args, sim_fun_args)

        idx_list <- get_idx_list(expand_dims, size_core_fun_args)
        list_of_values <- get_list_of_values(sorted_fun_args, core_fun_args, sim_fun_args, idx_list, fun_arg_values, state_lyr, params, core_args, sim_args, fun)

        fun_eval <- evaluate_function(fun, list_of_values, size_core_fun_args, fun_arg_values, expand_dims)
        list_evaluated_funs[[fun]] <- fun_eval
        # str_fun_array_list[[fun]] <- paste0(fun, "[", paste(expand_dims, collapse = ", "), "]")
    }

    update_twig_obj(twig_obj, fun_args_expanded, fun_args, twig_funs, list_evaluated_funs, str_fun_array_list, fun_arg_values, fun_arg_value_sizes)
}

update_fun_args <- function(fun_args) {
    fun_args[fun_args == "cycle_in_state"] <- "expanded_state"
    if ("expanded_state" %in% fun_args) {
        fun_args <- fun_args[fun_args != "state"]
    }
    return(fun_args)
}

get_expand_dims <- function(core_fun_args, sim_fun_args) {
    if (length(sim_fun_args) > 0) {
        return(c(core_fun_args, "sim"))
    } else {
        return(core_fun_args)
    }
}

get_sorted_fun_args <- function(fun_args, core_fun_args, sim_fun_args) {
    other_fun_args <- fun_args[!fun_args %in% core_fun_args & !fun_args %in% sim_fun_args]
    return(c(core_fun_args, sim_fun_args, other_fun_args))
}



update_twig_obj <- function(twig_obj, fun_args_expanded, fun_args, twig_funs, list_evaluated_funs, str_fun_array_list, fun_arg_values, fun_arg_value_sizes) {
    twig_obj$fun_args_expanded <- fun_args_expanded
    twig_obj$fun_args <- fun_args
    twig_obj$twig_funs <- twig_funs
    twig_obj$list_evaluated_funs <- list_evaluated_funs
    twig_obj$str_fun_array_list <- str_fun_array_list
    twig_obj$fun_arg_values <- fun_arg_values
    twig_obj$fun_arg_value_sizes <- fun_arg_value_sizes
}

get_arg_values <- function(twig_obj, used_core_args, sim_args, n_cycles) {
    fun_arg_values <- list()
    for (arg in used_core_args) {
        fun_arg_values[[arg]] <- get_fun_arg_values(twig_obj, arg, n_cycles = n_cycles)
    }
    if (length(sim_args) > 0) {
        fun_arg_values[["sim"]] <- 1:n_sims
    }
    return(fun_arg_values)
}

get_arg_value_sizes <- function(fun_arg_values, core_args, sim_args) {
    arg_value_sizes <- vector(mode = "integer", length = 0)
    for (arg in core_args) {
        arg_value_sizes[arg] <- length(fun_arg_values[[arg]])
    }
    if (length(sim_args) > 0) {
        arg_value_sizes[["sim"]] <- n_sims
    }
    return(arg_value_sizes)
}


get_sim_args <- function(params, all_args) {
    sim_args <- names(params)
    used_sim_args <- sim_args[sim_args %in% all_args]
}

get_idx_list <- function(expand_dims, size_core_fun_args) {
    dim_prev <- c(1, cumprod(size_core_fun_args))[-(length(size_core_fun_args) + 1)]
    dim_next <- c(rev(cumprod(rev(size_core_fun_args))), 1)[-1]
    names(dim_prev) <- names(dim_next) <- names(size_core_fun_args)

    idx_list <- list()
    for (arg in expand_dims) {
        idx_list[[arg]] <- rep(1:size_core_fun_args[arg], each = dim_prev[arg], times = dim_next[arg])
    }
    return(idx_list)
}

get_list_of_values <- function(sorted_fun_args, core_fun_args, sim_fun_args, idx_list, fun_arg_values, state_lyr, params, core_args, sim_args, fun) {
    list_of_values <- list()
    for (arg in sorted_fun_args) {
        if (arg %in% core_fun_args) {
            if (arg == "expanded_state") {
                list_of_values[["state"]] <- state_lyr$repeated_states[idx_list[[arg]]]
                list_of_values[["cycle_in_state"]] <- state_lyr$cycles_in_states[idx_list[[arg]]]
            } else {
                list_of_values[[arg]] <- fun_arg_values[[arg]][idx_list[[arg]]]
            }
        } else if (arg %in% sim_fun_args) {
            list_of_values[[arg]] <- params[idx_list[["sim"]], arg]
        } else {
            stop(
                "Argument ", arg, " passed to function", fun,
                " is not a core twig argument, or a parameter. Core arguments are: ", core_args,
                ". parameter variables are: ", sim_args, ". scalars need to be defined in the global environment and not passed to the function."
            )
        }
    }
    return(list_of_values)
}

