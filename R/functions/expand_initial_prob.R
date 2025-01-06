expand_initial_prob <- function(p0_funs, fun_args, eval_funs_p0, sim_args, arg_values, core_args, state_layer, n_sims, arg_value_sizes, hash_string) {
    # expand the initial probs
    # this one can be a single array of p0
    # we don't need to regenerate it for each sim, because it is likely to be small
    # state*decision*sim

    # functions ---------------------------------------------------------------
    # get all p0 function arguments 
    if (length(p0_funs) > 0){ # there are some functions in the initial probs
        p0_fun_args <- unique(unlist(fun_args[p0_funs]))
        p0_is_sim_dep <- any(p0_fun_args %in% sim_args)

        p0_core_args <- p0_fun_args[p0_fun_args %in% core_args]
        p0_allowable_args <- c("decision", "sim")
        if (p0_core_args != "decision" | length(p0_core_args) > 1) {
            stop(p0_core_args, "are used, but only -- decision -- and -- params -- variables are allowed in the arguments of the functions used in the initial probs init_probs")
        }
        p0_core_args <- c("state", p0_core_args)

        # if initial probs are sim dependent, add sim to the dimensions
        if (p0_is_sim_dep) {
            p0_dim <- c(p0_core_args, "sim")
        }

        # expand p0 functions -----------------------------------------------------

        p0_fun_values_expanded <- expand_p0_funs(p0_funs, fun_args, eval_funs_p0, sim_args, arg_values, p0_allowable_args, n_sims)

            p0_arg_values <- arg_values[p0_dim]
    p0_arg_value_sizes <- arg_value_sizes[p0_dim]

    } else {
        # p0_fun_values_expanded <- p0_funs
        p0_core_args <- "state"
        p0_arg_values <- arg_values[p0_core_args]
        p0_arg_value_sizes <- arg_value_sizes[p0_core_args]
    }

 

    # replace hash with hash_string -----------------------------------------
    p0_elements <- state_layer$expanded_init_probs
    p0_compl_idx <- which(p0_elements == hash_string) # "\"#\"")
    p0_elements[p0_compl_idx] <- hash_string
    n_compl <- length(p0_elements[p0_elements == hash_string])
    if (n_compl > 1) {
        stop("Only one complement allowed in the initial probs init_probs")
    }

    # for each element in the initial probs fill in p0_array -----------------------------------------

    p0_array <- array(0, dim = p0_arg_value_sizes, dimnames = p0_arg_values)
    p0_df <- expand.grid(p0_arg_values)
    p0_df$p0 <- 0
    for (i in seq_along(p0_elements)) {
        p0 <- p0_elements[i]
        sel_state <- p0_arg_values$state[i]
        #print(p0)
        if (is_numeric_string(p0)) { # if it is a numeric value just use that value
            p0_df$p0[p0_df$state == sel_state] <- as.numeric(p0)
        } else if (p0 %in% p0_funs) { # if it is a function first use the expanded function
            p0_df$p0[p0_df$state == sel_state] <- p0_fun_values_expanded[[p0]]
        } else if (is_global_variable(p0)) { # if it is a global variable, get its value
            tryCatch({
                p0_value <- get(p0)
                if (!is_scalar(p0_value)) {
                    stop("If ", p0, " is a fixed value, it must be a scalar")
                }
            }, error = function(e) {
                stop(p0, " is not defined")
            })
            # if p0_value is not a scalar, stop
            if (!is_scalar(p0_value)) {
            }
            p0_df$p0[p0_df$state == sel_state] <- get(p0)
        } else if (p0 == hash_string) {
            # can be a complement
        } else {
            stop(p0, "is not a numeric value, a function, \"#\" or a global variable")
        }
    }

    # compute the complement as the sum of the rest of the probs -------------
    # group the data frame by state and sum the probs

    p0 <- p0_df$p0
    if (n_compl == 1) {
        #p0_compl_state_idx <- which(arg_values["state"] p0_arg_values$state[p0_compl_idx]
        p0 <- matrix(p0, nrow = arg_value_sizes["state"])
        p0[p0_compl_idx, ] <- 1 - colSums(p0)
    } 

    # return the p0 array after expanding it so it is always S, D, sim -----------------------------------------------------------
    # 
    p0_allowable_args <- c("state", "decision", "sim")
    unsorted_dim_names <- c(p0_core_args, p0_allowable_args[p0_allowable_args != p0_core_args])

    p0_array_unsorted <- array(p0, dim = arg_value_sizes[unsorted_dim_names], dimnames = arg_values[unsorted_dim_names])
            # Create the permutation vector to reorder dim0 to match core_args
        perm <- match(p0_allowable_args, unsorted_dim_names)

        # Sort the IDX dims so they match the core arguments order
        p0_array <- aperm(p0_array_unsorted, perm)

    #p0_array <- array(p0, dim = p0_arg_value_sizes, dimnames = p0_arg_values)
    return(p0_array)
}
