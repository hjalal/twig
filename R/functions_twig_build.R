
#' Create a new twig
#'
#' @param model_type 
#' @param n_cycles = 50 
#' @return a new twig object 
#' @export
#' @importFrom magrittr %>%
#' @examples twig(model_type = "Markov", n_cycles = 40)
#' 
twig <- function() {
  twig_obj <- list()
  class(twig_obj) <- c("twig_decision", "twig_class")
  twig_obj
}


#' Define a method for the `+` operator for `twig` objects
#'
#' @param twig_obj 
#' @param layer 
#' @description Adds layers to the twig object
#' @return twig_obj
#' @export
#'
#' @examples 
#' mytwig <- twig(model_type = "Markov", n_cycles = 75) + 
#' decisions("StandardOfCare", "StrategyA", "StrategyB", "StrategyAB")

`+.twig_class` <- function(twig_obj, layer) {
  if (is.null(layer$type)){ 
    if (layer[[1]]$type=="states"){ # split the layer into 3
      for (l in layer){
        twig_obj$layers <- c(twig_obj$layers, list(l))
      }
      class(twig_obj) <- NULL
      class(twig_obj) <- c("twig_markov", "twig_class")
    } else if (layer[[1]]$type=="payoffs"){
      for (l in layer){
        twig_obj$layers <- c(twig_obj$layers, list(l))
      }
  }
  } else {
    # Add the layer to the twig object
    twig_obj$layers <- c(twig_obj$layers, list(layer))
  }
  # Return the modified twig object
  twig_obj
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
  input_string <- paste0(deparse(substitute(probs)), collapse = "")
  
  # payoffs_string <- paste0(deparse(substitute(payoffs)), collapse = "")
  # if (payoffs_string == "NULL"){
  #   payoffs_string <- ""
  # }
  #input_string <- as.list(match.call())$probs
  list(type = "event", 
       event = name, 
       values = scenarios, 
       probs = probs2string(input_string),
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
#' @examples discounts(payoffs = c("cost", "effectiveness"), discounts = c(0.5, 0.5))
#' @examples discounts(payoffs = c("cost", "effectiveness"), discounts = c(0.15, 0.15))


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
#' @param ... Markov state names
#'
#' @return a twig layer with Markov state names
#' @export
#'
#' @examples states("Healthy", "Sick", "Dead")
states <- function(names, init_probs, max_cycle_in_states = NULL){
  l1<- list(type = "states", states = names)
  l2<- list(type = "initial_prob", states = names, probs = init_probs)
  
  tunnel_names <- names[max_cycle_in_states > 1]
  tunnel_lengths <- max_cycle_in_states[max_cycle_in_states>1]
  
  if (length(tunnel_names)>0){
    l3<- list(type = "tunnels", states = tunnel_names, lengths = tunnel_lengths) #, max_cycle_in_states = max_cycle_in_states)
    l <- list(l1,l2,l3)
    
  } else {
    l <- list(l1,l2)
  }
  return(l)
}

#' Add final_outcomes from a decision tree to a twig
#'
#' @param ... Decision final_outcome names
#'
#' @return a twig layer with Decision final_outcome names
#' @export
#'
#' @examples final_outcomes("Alive", "Dead")
# final_outcomes <- function(...){
#   list(type = "final_outcomes", final_outcomes = c(...))
# }

#' Add payoffs to a twig
#'
#' @param ... a named list containing the payoffs and the associated payoff functions
#'
#' @return a twig layer with payoffs
#' @export
#'
#' @examples payoffs(cost = cost_function(state), effectiveness = effective_function(state))
payoffs <- function(...){
  input_string <- as.list(match.call())
  payoffs <- input_string[-1]
  payoffs$discount_rates <- NULL
  l1 <- list(type = "payoffs", payoffs = payoffs)
  discounts <- input_string$discount_rates
  if (length(discounts)>0){
    names(discounts) <- c(NA, names(payoffs))
    l2 <- list(type = "discounts", payoffs = names(payoffs), discounts = discounts)
    l <- list(l1,l2)
  } else {
    l <- l1
  }
return(l)
}




