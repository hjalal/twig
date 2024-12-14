
# gets function values from the twig. note that state is already combiend with cycle_in_state if it is a tunnel state.
get_fun_arg_values <- function(twig_obj, arg_name, n_cycles = NULL) {
  if (arg_name %in% c("decision", "cycle")) { # , "cycle_in_state")){
    arg_name <- paste0(arg_name, "s")

    #if (arg_name %in% c("states")) {
      # # Use lapply to filter the list based on the condition
      # index <- which(sapply(twig_obj$layers, function(x) "states" %in% x$type))
      # # Remove NULL elements from the list
      # lyr <- twig_obj$layers[[index]]
      # arg_values <- lyr$names
    if (arg_name %in% c("decisions")) {
      # Use lapply to filter the list based on the condition
      index <- which(sapply(twig_obj$layers, function(x) arg_name %in% x$type))
      # Remove NULL elements from the list
      lyr <- twig_obj$layers[[index]]
      arg_values <- lyr[[arg_name]]
    } else if (arg_name %in% c("cycles")) { # ,"cycle_in_states")){
      arg_values <- 1:n_cycles
    }
  } else if (arg_name %in% c("outcome")) {
    # only for decision trees
    events_df <- get_events_df(twig_obj)
    arg_values <- unique(events_df$goto[!events_df$goto %in% events_df$event])
  } else if (arg_name == "state") {
    # Use lapply to filter the list based on the condition
    index <- which(sapply(twig_obj$layers, function(x) "states" %in% x$type))
    # Remove NULL elements from the list
    lyr <- twig_obj$layers[[index]]
    tunnel_lengths <- lyr$tunnel_lengths
    states <- lyr$names
    states_expanded <- rep(states, tunnel_lengths)
    # Create expanded states only for states with tunnel_lengths > 1
    expanded_states <- lapply(1:length(states), function(i) {
      if (tunnel_lengths[i] > 1) {
        paste0(states[i], "_tnl", 1:tunnel_lengths[i])
      } else {
        states[i]
      }
    })
    # Flatten the list to a vector
    arg_values <- unlist(expanded_states)
  } else { # event name
    # Use lapply to filter the list based on the condition
    index <- which(sapply(twig_obj$layers, function(x) arg_name %in% x$event))
    lyr <- twig_obj$layers[[index]]
    arg_values <- lyr$values
  }
  return(arg_values)
}


# takes out function arguments from the function name definition.
get_function_arguments <- function(fun_name) {

    # For multiple functions, get unique arguments across all
    arguments <- lapply(fun_name, function(fname) {
      fun <- get(fname)
      names(formals(fun))

      # if there is cycle_in_state, replace it with expanded_state, and remove state
      # if ("cycle_in_state" %in% fun_args) {
      #   fun_args[fun_args == "cycle_in_state"] <- "expanded_state"
      #   fun_args <- fun_args[fun_args != "state"]
      # }
      #fun_args
    })
    names(arguments) <- fun_name

  return(arguments)
}



get_prob_funs <- function(twig_obj) {
  # Get events from twig object
  events <- retrieve_layer_by_type(twig_obj, type = "event")

  # Extract and flatten all probability values
  all_probs <- unlist(lapply(events, function(x) x$probs))

  # Remove '#' placeholder and get unique values
  unique_probs <- unique(all_probs[!all_probs %in% c('"#"', "complement")])

  filter_functions(unique_probs)
}

  # Filter to keep only elements that are functions
  filter_functions <- function(fun_names){
    fun_names[sapply(fun_names, function(fun_name) {
      exists(fun_name, mode = "function")
    })]
    #modified_names[modified_names == "cycle_in_state"] <- "expanded_state"
    #return(modified_names)
  }

get_reward_funs <- function(twig_obj) {
    # get payoffs
    payoffs <- retrieve_layer_by_type(twig_obj, type = "payoffs")
    filter_functions(payoffs$payoffs)
}

get_p0_funs <- function(twig_obj, type = NULL) {
  # get initial probabilities functions
  states <- retrieve_layer_by_type(twig_obj, type = "states")
  filter_functions(states$init_probs)
}


get_core_args <- function(twig_obj, all_args) {
  core_args <- c("decision", "state", "cycle", "expanded_state", "outcome")
  events <- retrieve_layer_by_type(twig_obj, type = "event")
  for (event in events) {
    core_args <- c(core_args, event$event)
  }
  core_args <- core_args[core_args %in% all_args]
  return(core_args)
}



evaluate_function <- function(twig_funs, fun_args, core_args, sim_args, arg_values, params) {
    fun_eval <- list()
    for (fun in twig_funs){
        sel_fun_args <- fun_args[[fun]]
        sel_core_args <- core_args[core_args %in% sel_fun_args]
        sel_sim_args <- sim_args[sim_args %in% sel_fun_args]
        if (length(sel_sim_args)>0){
            sel_core_args <- c(sel_core_args, "sim")
            sim_df <- params[, sel_sim_args, drop = FALSE]
        }
    # Extract values for sel_core_args from arg_values
    sel_arg_values <- arg_values[sel_core_args]
    
    # Create all possible permutations
    permutations <- expand.grid(sel_arg_values)
    if (length(sel_sim_args)>0){ # add the params samples using the sims
        permutations <- cbind(permutations, sim_df[permutations$sim,,drop = FALSE])
        permutations$sim <- NULL
    }

    #if ("expanded_state" %in% sel_fun_args){
        # Split the expanded_state column into state and cycle_in_statef
        split_columns <- strsplit(as.character(permutations$state), "_tnl")
        permutations$state <- sapply(split_columns, `[`, 1)
        if ("cycle_in_state" %in% sel_fun_args){
        permutations$cycle_in_state <- as.numeric(sapply(split_columns, `[`, 2))
        }
        #permutations$expanded_state <- NULL
    #}

    # Evaluate the function for each permutation
    

        fun_eval[[fun]] <- do.call(fun, as.list(permutations))
    }

    return(fun_eval)
}

get_sim_args <- function(params, all_args) {
    sim_args <- names(params)
    used_sim_args <- sim_args[sim_args %in% all_args]
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