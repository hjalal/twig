# nolint start

#source("R/functions/fun_in_twig.R")


# update_fun_args <- function(fun_args) {
#     fun_args[fun_args == "cycle_in_state"] <- "expanded_state"
#     if ("expanded_state" %in% fun_args) {
#         fun_args <- fun_args[fun_args != "state"]
#     }
#     return(fun_args)
# }

# get_expand_dims <- function(core_fun_args, sim_fun_args) {
#     if (length(sim_fun_args) > 0) {
#         return(c(core_fun_args, "sim"))
#     } else {
#         return(core_fun_args)
#     }
# }

# get_sorted_fun_args <- function(fun_args, core_fun_args, sim_fun_args) {
#     other_fun_args <- fun_args[!fun_args %in% core_fun_args & !fun_args %in% sim_fun_args]
#     return(c(core_fun_args, sim_fun_args, other_fun_args))
# }



# update_twig_obj <- function(twig_obj, fun_args_expanded, fun_args, twig_funs, list_evaluated_funs, str_fun_array_list, fun_arg_values, fun_arg_value_sizes) {
#     twig_obj$fun_args_expanded <- fun_args_expanded
#     twig_obj$fun_args <- fun_args
#     twig_obj$twig_funs <- twig_funs
#     twig_obj$list_evaluated_funs <- list_evaluated_funs
#     twig_obj$str_fun_array_list <- str_fun_array_list
#     twig_obj$fun_arg_values <- fun_arg_values
#     twig_obj$fun_arg_value_sizes <- fun_arg_value_sizes
# }







# get_idx_list <- function(expand_dims, size_core_fun_args) {
#     dim_prev <- c(1, cumprod(size_core_fun_args))[-(length(size_core_fun_args) + 1)]
#     dim_next <- c(rev(cumprod(rev(size_core_fun_args))), 1)[-1]
#     names(dim_prev) <- names(dim_next) <- names(size_core_fun_args)

#     idx_list <- list()
#     for (arg in expand_dims) {
#         idx_list[[arg]] <- rep(1:size_core_fun_args[arg], each = dim_prev[arg], times = dim_next[arg])
#     }
#     return(idx_list)
# }

# get_list_of_values <- function(sorted_fun_args, core_fun_args, sim_fun_args, idx_list, fun_arg_values, state_lyr, params, core_args, sim_args, fun) {
#     list_of_values <- list()
#     for (arg in sorted_fun_args) {
#         if (arg %in% core_fun_args) {
#             if (arg == "expanded_state") {
#                 list_of_values[["state"]] <- state_lyr$repeated_states[idx_list[[arg]]]
#                 list_of_values[["cycle_in_state"]] <- state_lyr$cycles_in_states[idx_list[[arg]]]
#             } else {
#                 list_of_values[[arg]] <- fun_arg_values[[arg]][idx_list[[arg]]]
#             }
#         } else if (arg %in% sim_fun_args) {
#             list_of_values[[arg]] <- params[idx_list[["sim"]], arg]
#         } else {
#             stop(
#                 "Argument ", arg, " passed to function", fun,
#                 " is not a core twig argument, or a parameter. Core arguments are: ", core_args,
#                 ". parameter variables are: ", sim_args, ". scalars need to be defined in the global environment and not passed to the function."
#             )
#         }
#     }
#     return(list_of_values)
# }

