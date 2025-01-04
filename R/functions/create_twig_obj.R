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
  twig_obj <- list() #new.env()
  class(twig_obj) <- c("decision_twig", "twig_class")
  return(twig_obj)
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
#' twig_obj <- twig(model_type = "Markov", n_cycles = 75) + 
#' decisions("StandardOfCare", "StrategyA", "StrategyB", "StrategyAB")
`+.twig_class` <- function(twig_obj, layer) {

  # change model class if states layer is added
  if (layer$type == "states") {
    # Since states are defined, treat this as a Markov model
    message("Note: A states layer detected in your twig - treating Twig as a Markov model. 
            For a decision tree, make sure to remove the states layer.")
    class(twig_obj) <- NULL 
    class(twig_obj) <- c("markov_twig", "twig_class")
  }
  
  # Add the layer to the twig object
  twig_obj$layers <- c(twig_obj$layers, list(layer))
  
  # Return the modified twig object
  return(twig_obj)
}




#' Add event mapping
#'
#' @param name 
#' @param options 
#' @param probs 
#' @param transitions 
#'
#' @return twig layer 
#' @export
#'
#' @examples event_mapping(name = "event_progress", 
#' options = c(TRUE,FALSE), 
#' probs = c(p_progress_function(state), Inf), 
#' transitions = c("Severe","stay")
#' 
event <- function(name, options, probs, transitions){
  # if (!is.character(name)) {
  #   name <- as.character(name)  # Convert to character string if not already
  # }
  
  name <- deparse(substitute(name))
  options <- sapply(substitute(options), deparse)
  probs <- sapply(substitute(probs), deparse)
  transitions <- sapply(substitute(transitions), deparse)
  
  options <- remove_quotes(options)  # Remove leading and trailing quotes
  probs <- remove_quotes(probs)  # Remove leading and trailing quotes
  transitions <- remove_quotes(transitions)  # Remove leading and trailing quotes
  name <- remove_quotes(name)  # Remove leading and trailing quotes
  
  # events are the links that can either go to states or other events
  #input_string <- paste0(deparse(substitute(probs)), collapse = "")
  
  # payoffs_string <- paste0(deparse(substitute(payoffs)), collapse = "")
  # if (payoffs_string == "NULL"){
  #   payoffs_string <- ""
  # }
  #input_string <- as.list(match.call())$probs
  list(type = "event", 
       event = name, 
       options = options, 
       probs = probs, #2string(input_string),
       transitions = transitions #,
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
decisions <- function(names){
  names <- sapply(substitute(names), deparse)
  
  names <- remove_quotes(names)  # Remove leading and trailing quotes
  
  list(type = "decisions", decisions = names)
  # Define decisions based on each input
}

# Helper function to convert expressions to strings without extra quotes
# Note: expr should already be substituted
to_strings <- function(expr_substituted) {
  if (is.call(expr_substituted)) {
    noquote(sapply(expr_substituted[-1], deparse))
  } else {
    noquote(deparse(expr_substituted))
  }
}

#' Add Markov states to a twig
#'
#' @param names ... Markov state names
#' @param init_probs ... initial probs 
#' @param max_cycles ... optional max tunnel lenghts. If ignored a length of 1 is assumed.
#'
#' @return a twig layer with Markov state names
#' @export
#'
#' @examples states("Healthy", "Sick", "Dead")
states <- function(names, init_probs, max_cycles = NULL) {
  # Convert init_probs to character while preserving unevaluated expressions
  names <- sapply(substitute(names), deparse)
  
  names <- remove_quotes(names)  # Remove leading and trailing quotes
  
    if ("stay" %in% names) {
      stop("Error: 'stay' cannot be used as a state name in the states layer.")
    }

  init_probs <- to_strings(substitute(init_probs))
  if (length(init_probs) != length(names)) {
    stop("Error: 'init_probs' must have the same length as state 'names'.",
         "state names: ", length(names), ", init_probs: ", length(init_probs))
  }
  if (is.null(max_cycles)) {
    max_cycles <- rep(1, length(names))
  } else if (length(max_cycles) != length(names)) {
    stop("Error: 'max_cycles' must have the same length as state 'names'.",
         "state names: ", length(names), ", max_cycles: ", length(max_cycles))
  }

  # For states with tunnel length > 1, get cycles and names
  cycles_in_states <- unlist(sapply(max_cycles, seq_len))
  repeated_tunnels <- rep(max_cycles, max_cycles)
  
  repeated_states <- rep(names, max_cycles)
  tunneled_states <- ifelse(repeated_tunnels > 1, paste0(repeated_states, "_tnl", cycles_in_states), repeated_states)
  
  expanded_init_probs <- rep(0, length(cycles_in_states))
  expanded_init_probs[cycles_in_states == 1] <- init_probs

  # remove cycles_in_states for states with tunnel length of 1
  cycles_in_states[repeated_tunnels == 1] <- NA
  l1 <- list(type = "states",
            names = names,
            init_probs = init_probs,
            max_cycles = max_cycles,
            expanded_init_probs = expanded_init_probs,
            cycles_in_states = cycles_in_states,
            #repeated_tunnels = repeated_tunnels,
            repeated_states = repeated_states,
            tunneled_states = tunneled_states)
  
  return(l1)
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
payoffs <- function(names, discount_rates=NULL){
  names <- sapply(substitute(names), deparse)
  
  names <- remove_quotes(names) 
  # gsub('^"|"$', '', names)  # Remove leading and trailing quotes
  # if (length(names>1)){
  #   names <- names[-1]
  # }
  
  if (is.null(discount_rates)){ 
    discount_rates <- rep(0, length(names))
  }
  names(discount_rates) <- names
  l <- list(type = "payoffs", 
            payoffs = names, 
            discount_rates = discount_rates)
  return(l)
}

remove_quotes <- function(x){
  x <- gsub('^"|"$', '', x)  # Remove leading and trailing quotes
  if (length(x)>1){
    x <- x[-1]
  }
  return(x)
}


