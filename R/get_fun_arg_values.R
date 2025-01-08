
get_fun_arg_values <- function(twig_obj, arg_name, n_cycles = NULL) {
  if (arg_name %in% c("decision", "cycle")) { 
    arg_name <- paste0(arg_name, "s")

    
      
      
      
      
      
    if (arg_name %in% c("decisions")) {
      
      index <- which(sapply(twig_obj$layers, function(x) arg_name %in% x$type))
      
      lyr <- twig_obj$layers[[index]]
      arg_values <- lyr[[arg_name]]
    } else if (arg_name %in% c("cycles")) { 
      arg_values <- 1:n_cycles
    }
  } else if (arg_name %in% c("outcome")) {
    
    events_df <- get_events_df(twig_obj)
    arg_values <- unique(events_df$transitions[!events_df$transitions %in% events_df$event])
  } else if (arg_name == "state") {
    
    index <- which(sapply(twig_obj$layers, function(x) "states" %in% x$type))
    
    lyr <- twig_obj$layers[[index]]
    max_cycles <- lyr$max_cycles
    states <- lyr$names
    states_expanded <- rep(states, max_cycles)
    
    expanded_states <- lapply(1:length(states), function(i) {
      if (max_cycles[i] > 1) {
        paste0(states[i], "_tnl", 1:max_cycles[i])
      } else {
        states[i]
      }
    })
    
    arg_values <- unlist(expanded_states)
  } else { 
    
    index <- which(sapply(twig_obj$layers, function(x) arg_name %in% x$event))
    lyr <- twig_obj$layers[[index]]
    arg_values <- lyr$options
  }
  return(arg_values)
}
