# expand the initial transition probs
# this one can be a single array of p0
# we don't need to regenerate it for each sim, because it is likely to be small
# state*decision*sim

# functions ---------------------------------------------------------------

# get all p0 function arguments 
p0_fun_args <- unique(unlist(fun_args[p0_funs]))
p0_is_sim_dep <- any(p0_fun_args %in% sim_args)

p0_core_args <- p0_fun_args[p0_fun_args %in% core_args]
p0_allowable_args <- c("decision", "sim")

if (p0_core_args != "decision" | length(p0_core_args) > 1){
    stop(p0_core_args, "are used, but only decision is allowed in the arguments of the functions used in the initial probabilities init_probs")
}
p0_core_args <- c("state", p0_core_args)

# if initial probabilities are sim dependent, add sim to the dimensions
if (p0_is_sim_dep){
    p0_dim <- c(p0_core_args, "sim")
}

# expand p0 functions -----------------------------------------------------

p0_fun_values_expanded <- expand_p0_funs(p0_funs, fun_args, eval_funs, sim_args, arg_values, p0_allowable_args, n_sims)


# replace hash with "COMPLEMENT" -----------------------------------------
p0_elements <- state_layer$expanded_init_probs
p0_compl_idx <- which(p0_elements == "\"#\"")
p0_elements[p0_compl_idx] <- "COMPLEMENT" 
n_compl <- length(p0_elements[p0_elements == "COMPLEMENT"])
if (n_compl > 1){
    stop("Only one complement allowed in the initial probabilities init_probs")
}


# for each element in the intial probs fill in p0_array -----------------------------------------
p0_arg_values <- arg_values[p0_dim]
p0_arg_value_sizes <- arg_value_sizes[p0_dim]
p0_array <- array(0, dim = p0_arg_value_sizes, dimnames = p0_arg_values)
p0_df <- expand.grid(p0_arg_values)
p0_df$p0 <- 0
for (i in seq_along(p0_elements)){
    p0 <- p0_elements[i]
    sel_state <- p0_arg_values$state[i]
    #print(p0)
    if (is_numeric_string(p0)){ # if it is a numeric value just use that value
        p0_df$p0[p0_df$state==sel_state] <- as.numeric(p0)
    } else if (p0 %in% p0_funs){ # if it is a function first use the expanded function
        p0_df$p0[p0_df$state==sel_state] <- p0_fun_values_expanded[[p0]]
    } else if (is_global_variable(p0)){ # if it is a global variable, get its value
        p0_value <- get(p0)
        # if p0_value is not a scalar, stop
        if (!is_scalar(p0_value)){
            stop(p0, "is must be a scalar value. Otherwise define it as a function of the decision and simulation parameters")
        }
        p0_df$p0[p0_df$state==sel_state] <- get(p0)
    } else if (p0 != "COMPLEMENT"){
        stop(p0, "is not a numeric value, a function, \"#\" or a global variable")
    }
}

# compute the complement as the sum of the rest of the probabilities -------------
# group the data frame by state and sum the probabilities

if (n_compl == 1){
    p0_compl_state <- p0_arg_values$state[p0_compl_idx]
    group_vars <- lapply(p0_dim[p0_dim != "state"], function(var) p0_df[[var]])
    p0_compl <- aggregate(p0_df$p0, by = group_vars, FUN = sum)
    p0_df[p0_df$state == p0_compl_state, "p0"] <- 1 - p0_compl$x
}

# return the p0 array -----------------------------------------------------------
p0_array <- array(p0_df$p0, dim = p0_arg_value_sizes, dimnames = p0_arg_values)


