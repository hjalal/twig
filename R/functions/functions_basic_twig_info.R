# basic twig info
add_decision_info <- function(twig_obj){
  # retrieve states layer
  decision_layer <- retrieve_layer_by_type(twig_obj, type = "decisions")
  twig_obj$decisions <- decision_layer$decisions
  twig_obj$n_decisions <- length(twig_obj$decisions)
  return(twig_obj)
}

event_mapping_info <- function(twig_obj){
  # retrieve states layer
  event_layer <- retrieve_layer_by_type(twig_obj, type = "events")
  twig_obj$events <- event_layer$events
  twig_obj$n_events <- length(twig_obj$events)
  return(twig_obj) 
}

add_final_outcome_info <- function(twig_obj){
  # retrieve states layer
  #final_outcomes_layer <- retrieve_layer_by_type(twig_obj, type = "final_outcomes")
  get_events_df(twig_obj)
  twig_obj$final_outcomes <- get_final_outcomes(events_df)
  twig_obj$n_final_outcomes <- length(twig_obj$final_outcomes)
  #return(twig_obj)
}
add_event_info <- function(twig_obj){
  # retrieve states layer
  #events_layer <- retrieve_layer_by_type(twig_obj, type = "events")
  events_df <- get_events_df(twig_obj)
  twig_obj$events <- get_events(events_df)
  twig_obj$n_events <- length(twig_obj$events)
  return(twig_obj)
}
# event_mapping_info <- function(twig_obj){
#   # retrieve states layer
#   event_layer <- retrieve_layer_by_type(twig_obj, type = "events")
#   twig_obj$events <- event_layer$events
#   twig_obj$n_events <- length(twig_obj$events)
#   return(twig_obj) 
# }

add_payoffs <- function(twig_obj){
  payoffs_layer <- retrieve_layer_by_type(twig_obj, type = "payoffs")
  twig_obj$payoff_names <- payoffs_layer$payoffs
  twig_obj$n_payoffs <- length(twig_obj$payoffs)
  twig_obj$discounts <- payoffs_layer$discount_rates
}

add_prob_funs <- function(twig_obj){
  twig_obj$prob_funs <- twig_obj$twig_funs[!twig_obj$twig_funs %in% twig_obj$payoff_names]
  twig_obj$prob_fun_args_expanded <- twig_obj$fun_args_expanded[twig_obj$prob_funs]
  twig_obj$is_cycle_dep <- "cycle" %in% unique(unlist(twig_obj$prob_fun_args_expanded))
}

retrieve_obj_type <- function(twig_obj, obj){
  states <- twig_obj$states
  events <- twig_obj$events
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

retrieve_layer_by_type <- function(twig_obj, type){
  # Use lapply to filter the list based on the condition
  outcome <- lapply(twig_obj$layers, function(x) if (x$type == type) x else NULL)
  # Remove NULL elements from the list
  lyr<-Filter(Negate(is.null), outcome)
  if (length(lyr)==1 & type != "event"){
    lyr <- lyr[[1]] #only select the first element if there is no more
  }
  lyr
}

