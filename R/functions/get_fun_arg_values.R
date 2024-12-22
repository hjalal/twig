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