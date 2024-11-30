# basic twig info
add_decision_info <- function(twig_env){
  # retrieve states layer
  decision_layer <- retrieve_layer_by_type(twig_env, type = "decisions")
  twig_env$decisions <- decision_layer$decisions
  twig_env$n_decisions <- length(twig_env$decisions)
  return(twig_env)
}

event_mapping_info <- function(twig_env){
  # retrieve states layer
  event_layer <- retrieve_layer_by_type(twig_env, type = "events")
  twig_env$events <- event_layer$events
  twig_env$n_events <- length(twig_env$events)
  return(twig_env) 
}

add_discounts_info <- function(twig_env){
  discounts_info <- retrieve_layer_by_type(twig_env, type = "discounts")
  if (is.null(discounts_info$discounts)){
    twig_env$discounts <- rep(0, twig_env$n_payoffs)
    names(twig_env$discounts) <- twig_env$payoff_names
    
  } else {
    twig_env$discounts <- discounts_info$discounts
    names(twig_env$discounts) <- c(NA, discounts_info$payoffs)
  }
  return(twig_env)
}

add_final_outcome_info <- function(twig_env){
  # retrieve states layer
  #final_outcomes_layer <- retrieve_layer_by_type(twig_env, type = "final_outcomes")
  events_df <- get_events_df(twig_env)
  twig_env$final_outcomes <- get_final_outcomes(events_df)
  twig_env$n_final_outcomes <- length(twig_env$final_outcomes)
  return(twig_env)
}
add_event_info <- function(twig_env){
  # retrieve states layer
  #events_layer <- retrieve_layer_by_type(twig_env, type = "events")
  events_df <- get_events_df(twig_env)
  twig_env$events <- get_events(events_df)
  twig_env$n_events <- length(twig_env$events)
  return(twig_env)
}
# event_mapping_info <- function(twig_env){
#   # retrieve states layer
#   event_layer <- retrieve_layer_by_type(twig_env, type = "events")
#   twig_env$events <- event_layer$events
#   twig_env$n_events <- length(twig_env$events)
#   return(twig_env) 
# }

add_payoffs <- function(twig_env){
  payoffs_layer <- retrieve_layer_by_type(twig_env, type = "payoffs")
  twig_env$payoffs <- payoffs_layer$payoffs
  twig_env$payoff_names <- names(payoffs_layer$payoffs)
  twig_env$n_payoffs <- length(twig_env$payoffs)
  return(twig_env) 
}

retrieve_obj_type <- function(twig_env, obj){
  states <- twig_env$states
  events <- twig_env$events
  if (obj %in% states){
    "state"
  } else if (obj %in% events){
    "event"
  } else if (obj %in% events & obj %in% states){
    stop(paste(obj, "cannot be both a state and an event"))
  } else if (!(obj %in% events) & !(obj %in% states)){
    stop(paste(obj, "is neither a state nor an event"))
  }
}

retrieve_layer_by_type <- function(twig_env, type){
  # Use lapply to filter the list based on the condition
  outcome <- lapply(twig_env$layers, function(x) if (x$type == type) x else NULL)
  # Remove NULL elements from the list
  lyr<-Filter(Negate(is.null), outcome)
  if (length(lyr)==1 & type != "event"){
    lyr <- lyr[[1]] #only select the first element if there is no more
  }
  lyr
}
