expand_initial_prob <- function(p0_funs, fun_args, eval_funs_p0, sim_args, arg_values, core_args, state_layer, n_sims, arg_value_sizes, hash_string) {

    if (length(p0_funs) > 0){ 
        p0_fun_args <- unique(unlist(fun_args[p0_funs]))
        p0_is_sim_dep <- any(p0_fun_args %in% sim_args)

        p0_core_args <- p0_fun_args[p0_fun_args %in% core_args]
        p0_allowable_args <- c("decision", "sim")
        if (p0_core_args != "decision" | length(p0_core_args) > 1) {
            stop(p0_core_args, "are used, but only -- decision -- and -- params -- variables are allowed in the arguments of the functions used in the initial probs init_probs")
        }
        p0_core_args <- c("state", p0_core_args)

        if (p0_is_sim_dep) {
            p0_dim <- c(p0_core_args, "sim")
        } else {
            p0_dim <- p0_core_args
        }

        p0_fun_values_expanded <- expand_p0_funs(p0_funs, fun_args, eval_funs_p0, sim_args, arg_values, p0_allowable_args, n_sims)

            p0_arg_values <- arg_values[p0_dim]
    p0_arg_value_sizes <- arg_value_sizes[p0_dim]

    } else {

        p0_core_args <- "state"
        p0_arg_values <- arg_values[p0_core_args]
        p0_arg_value_sizes <- arg_value_sizes[p0_core_args]
    }

    p0_elements <- state_layer$expanded_init_probs
    p0_compl_idx <- which(p0_elements == hash_string) 
    p0_elements[p0_compl_idx] <- hash_string
    n_compl <- length(p0_elements[p0_elements == hash_string])
    if (n_compl > 1) {
        stop("Only one complement allowed in the initial probs init_probs")
    }

    p0_array <- array(0, dim = p0_arg_value_sizes, dimnames = p0_arg_values)
    p0_df <- expand.grid(p0_arg_values)
    p0_df$p0 <- 0
    for (i in seq_along(p0_elements)) {
        p0 <- p0_elements[i]
        sel_state <- p0_arg_values$state[i]

        if (is_numeric_string(p0)) { 
            p0_df$p0[p0_df$state == sel_state] <- as.numeric(p0)
        } else if (p0 %in% p0_funs) { 
            p0_df$p0[p0_df$state == sel_state] <- p0_fun_values_expanded[[p0]]
        } else if (is_global_variable(p0)) { 
            tryCatch({
                p0_value <- get(p0)
                if (!is_scalar(p0_value)) {
                    stop("If ", p0, " is a fixed value, it must be a scalar")
                }
            }, error = function(e) {
                stop(p0, " is not defined")
            })

            if (!is_scalar(p0_value)) {
            }
            p0_df$p0[p0_df$state == sel_state] <- get(p0)
        } else if (p0 == hash_string) {

        } else {
            stop(p0, "is not a numeric value, a function, or a global variable.")
        }
    }

    p0 <- p0_df$p0
    if (n_compl == 1) {

        p0 <- matrix(p0, nrow = arg_value_sizes["state"])
        p0[p0_compl_idx, ] <- 1 - colSums(p0)
    } 

    p0_allowable_args <- c("state", "decision", "sim")
    unsorted_dim_names <- c(p0_core_args, p0_allowable_args[!p0_allowable_args %in% p0_core_args])

    # arg_value_sizes and arg_values omit "sim" when no parameter is used by any
    # function. The initial-probability array still needs a sim dimension so it
    # stays 3D (state x decision x sim) for the downstream trace construction;
    # fall back to n_sims for its size (and a missing core arg to size 1).
    dim_sizes <- vapply(unsorted_dim_names, function(d) {
        s <- unname(arg_value_sizes[d])
        if (is.na(s)) (if (d == "sim") as.numeric(n_sims) else 1) else as.numeric(s)
    }, numeric(1))
    names(dim_sizes) <- unsorted_dim_names
    dim_names_list <- lapply(unsorted_dim_names, function(d) arg_values[[d]])
    names(dim_names_list) <- unsorted_dim_names

    p0_array_unsorted <- array(p0, dim = dim_sizes, dimnames = dim_names_list)

        perm <- match(p0_allowable_args, unsorted_dim_names)

        p0_array <- aperm(p0_array_unsorted, perm)

    return(p0_array)
}
