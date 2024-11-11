
#' Create a new twig
#'
#' @return a new twig object 
#' @export
#' @import data.table
#' @examples twig(model_type = "Markov", n_cycles = 40)
#' 
twig <- function() {
  twig <- list()
  class(twig) <- c("twig_decision", "twig_class")
  twig
}


#' Define a method for the `+` operator for `twig` objects
#'
#' @param twig 
#' @param layer 
#' @description Adds layers to the twig object
#' @return twig
#' @export
#'
#' @examples 
#' twig <- twig() + 
#' decisions("StandardOfCare", "StrategyA", "StrategyB", "StrategyAB")

`+.twig_class` <- function(twig, layer) {
  if (is.null(layer$type)){ 
    if (layer[[1]]$type=="states"){ # split the layer into 3
      for (l in layer){
        twig$layers <- c(twig$layers, list(l))
      }
      class(twig) <- NULL
      class(twig) <- c("twig_markov", "twig_class")
    } else if (layer[[1]]$type=="payoffs"){
      for (l in layer){
        twig$layers <- c(twig$layers, list(l))
      }
  }
  } else {
    # Add the layer to the twig object
    twig$layers <- c(twig$layers, list(layer))
  }
  # Return the modified twig object
  twig
}



#' Add event mapping
#'
#' @param name 
#' @param scenarios 
#' @param probs 
#' @param goto 
#'
#' @return twig layer 
#' @export
#'
#' @examples event_mapping(name = "event_progress", 
#' scenarios = c(TRUE,FALSE), 
#' probs = c(p_progress_function(state), Inf), 
#' goto = c("Severe","curr_state")
#' 
event <- function(name, scenarios, probs, goto){
  # events are the links that can either go to states or other events
  # input_string <- paste0(deparse(substitute(probs)), collapse = "")
  
  # payoffs_string <- paste0(deparse(substitute(payoffs)), collapse = "")
  # if (payoffs_string == "NULL"){
  #   payoffs_string <- ""
  # }
  #input_string <- as.list(match.call())$probs
  list(type = "event", 
       event = name, 
       values = scenarios, 
       probs = probs, #probs2string(input_string),
       goto = goto #,
       #payoffs = payoffs_string
  )
}

#' Add discounts to a twig Markov object
#'
#' @param ... decision names
#'
#' @return a twig layer with decision names
#' @export
#'
#' @examples discounts(names = c("cost", "effectiveness"), discount_rates = c(0.5, 0.5))


#' Title
#'
#' @param states names of the states to expand
#' @param lengths the length of each tunnel state
#'
#' @return a twig layer with tunnels
#' @export
#'
#' @examples tunnels(states = c("S1", "S2"), lengths = c(3, 5))



#' Add decisions to a twig
#'
#' @param ... decision names
#'
#' @return a twig layer with decision names
#' @export
#'
#' @examples decisions("A", "B", "C")
decisions <- function(...){
  list(type = "decisions", decisions = c(...))
  # Define decisions based on each input
}


#' Add Markov states to a twig
#'
#' @param names ... Markov state names
#' @param init_probs ... initial probabilities 
#' @param tunnel_lengths ... optional max tunnel lenghts. If ignored a length of 1 is assumed.
#'
#' @return a twig layer with Markov state names
#' @export
#'
#' @examples states("Healthy", "Sick", "Dead")
states <- function(names, init_probs, tunnel_lengths = NULL){
  if (is.null(tunnel_lengths)){
    tunnel_lengths <- rep(1, length(names))
  } 
  #Expand each element from 1 to the value of the element
  expanded_lengths <- unlist(sapply(tunnel_lengths, function(x) 1:x))
  
  states <- rep(names, tunnel_lengths)
  init_probs <- rep(init_probs, tunnel_lengths)
  tunnel_lengths <- rep(tunnel_lengths, tunnel_lengths)
  #expanded_lengths <- unlist(sapply(tunnel_lengths, function(x) 1:x))
 
  
  dt_states <- data.table(states, init_probs, expanded_lengths, tunnel_lengths)
  # Apply the conditional logic using ifelse to create the expanded_state and state2 columns
  dt_states[, `:=`(
    state = ifelse(tunnel_lengths > 1, 
                            paste0(states, "_tnl", expanded_lengths), 
                            states),
    state2 = ifelse(tunnel_lengths > 1, 
               paste0(states, "_tnl", 
                      ifelse(expanded_lengths < tunnel_lengths, expanded_lengths + 1, tunnel_lengths)), 
               states),
    x = ifelse(expanded_lengths>1, 0, init_probs)
  )]
  dt_curr_states <- dt_states[,.(state, state2)]
  dt_p0 <- dt_states[,.(state, x)]
  
  l1<- list(type = "states", 
            expanded_states = dt_p0$state,
            dt_p0 = dt_p0,
            dt_curr_states = dt_curr_states
            )
  return(l1)
}


#' Add payoffs to a twig
#'
#' @param names ... a named list containing the payoff function names
#' @param discount_rates ... an optional numeric vector containing discount rates per cycle. If null 0 is used
#'
#' @return a twig layer with payoffs
#' @export
#'
#' @examples payoffs(cost = cost_function(state), effectiveness = effective_function(state))
payoffs <- function(names, discount_rates = NULL){
  
  if (is.null(discount_rates)){
    discount_rates <- rep(0, length(names))
  }
  names(discount_rates) <- names
  l <- list(type = "payoffs", payoffs = names, discount_rates = discount_rates)
return(l)
}



#' Add payoffs to a twig
#'
#' @param twig ... a twig object generated with twig() 
#' @param params ... a list or data.table containing the PSA model parameters (rows=samples, columns=variables)
#' @param n_cycles ... number of cycles
#' @param return_prob ... whether to return the probabilities
#' @param return_trace ... whether to return the trace 
#' @param return_total_payoff ... whether to return total payoffs 
#' @param check_prob_add_to_one ... whether to check the results of the probabilities to make sure they add to 1
#' @param return_mean_payoff ... wheahter to return the mean of payoffs over all PSA simulations
#'
#' @return a list of results
#' @export
#'
#' @examples run_twig(twig=twig, params=params, n_cycles=10)
run_twig <- function(twig, 
                     params=NULL, 
                     n_cycles=NULL, 
                     return_prob=FALSE,
                     return_trace=FALSE,
                     return_total_payoff=FALSE,
                     check_prob_add_to_one=FALSE,
                     return_mean_payoff=TRUE){
  
  x_cols <- paste0("sim_", 1:n_sims)
  
  result_list <- list() 
  # list2env(params, envir = .GlobalEnv)
  list_fun_outputs <- twig_expand_functions(twig, params = params)
  #str(list_fun_outputs)
  
  fun_names <- list_fun_outputs$fun_names
  arg_values <- list_fun_outputs$arg_values
  fun_outputs <- list_fun_outputs$fun_outputs
  
  #View(fun_outputs$pDie)
  #unique_values <- get_unique_values(sel_fun_outputs)
  
  
  events_dt <- get_event_df(twig)
  
  #events_dt
  all_events <- unique(events_dt$event)
  twig_dims <- get_twig_dims(twig, events_dt, n_cycles, n_sims)
  states_layers <- retrieve_layer_by_type(twig, type = "states") 
  dt_curr_states <- states_layers$dt_curr_states
  
  # get segment probabilities
  dt_prob_list <- get_seg_probs(events_dt, fun_outputs, x_cols)
  
  # get path data frame that lists the events and their outcomes
  path_dt <- get_path_dt(events_dt, dt_curr_states, all_events, twig_dims)
  event_cols <- names(path_dt)
  event_cols <- event_cols[!event_cols %in% c("path_id", "dest", "seg_ids")]
  
  # get path probabilities
  dt_pathprob_list <- get_path_probs(path_dt, dt_prob_list, dt_curr_states, x_cols)
  
  # sum over all paths + curr_state 
  trans_probs <- smart_sum(dt_pathprob_list, x_cols)
  
  # Create trace 
  p0 <- copy(get_dt_p0(states_layers, n_sims, x_cols))
  Trace <- get_trace(trans_probs, p0, n_cycles, x_cols)
  
  # apply rewards ========
  payoff_results <- compute_payoffs(twig, event_cols, fun_outputs, path_dt, dt_pathprob_list, Trace, x_cols)
  
  if(check_prob_add_to_one){
    # Sum each of the sim_ columns by the other variables except 'state2'
    sum_x_by_group <- trans_probs[, 
                                  lapply(.SD, sum, na.rm = TRUE), 
                                  .SDcols = x_cols, 
                                  by = .(state, decision, cycle)  # Grouping by other variables except state2
    ]
    result_list[["sum_prob"]] <- sum_x_by_group
  }
  if(return_prob){
    result_list[["trans_prob"]] <- trans_probs
  }
  if(return_trace){
    result_list[["trace"]] <- Trace
  }
  if(return_mean_payoff){
    result_list[["mean_payoff"]] <- payoff_results$mean_payoff
  }
  if(return_total_payoff){
    result_list[["total_payoff"]] <- payoff_results$payoff_trace_list
    
  }
  return(result_list)
}

#' Title
#'
#' @param rate 
#' @description converts rate to probability using prob = 1-exp(-rate)
#' @return probability 
#' @export
#'
#' @examples
#' rate2prob(rate = 0.3)
rate2prob <- function(rate){
  1 - exp(-rate)
}

#' Title
#'
#' @param prob 
#' @description converts prob to rate using rate = -log(1-prob)
#' @return rate 
#' @export
#'
#' @examples
#' prob2rate(prob = 0.5)
prob2rate <- function(prob){
  -log(1-prob)
}
