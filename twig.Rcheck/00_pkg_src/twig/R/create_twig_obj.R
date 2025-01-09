#' Create a new twig object
#'
#' This function initializes a new twig object, which can be used to build Markov models and decision trees.
#'
#' @return A new twig object of class `decision_twig` and `twig_class` by default.
#' @export
#' @examples
#' twig_obj <- twig()
#' # see vignettes for more
twig <- function() {
  twig_obj <- list() 
  class(twig_obj) <- c("decision_twig", "twig_class")
  return(twig_obj)
}

#' Define a method for the `+` operator for `twig` objects
#'
#' This method allows layers to be added to a twig object using the `+` operator.
#'
#' @param twig_obj A twig object created by the `twig` function.
#' @param layer A layer to be added to the twig object.
#' @description Adds layers to the twig object. If a states layer is added, the twig object is treated as a Markov model.
#' @return The modified twig object with the new layer added.
#' @export
#' @examples 
#' twig_obj <- twig() + 
#'   decisions(names = c(StandardOfCare, StrategyA, StrategyB, StrategyAB))
`+.twig_class` <- function(twig_obj, layer) {
  # change model class if states layer is added
  if (layer$type == "states") {
    # Since states are defined, change class to a Markov model
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

#' Add an event layer to a twig object
#'
#' This function creates an event layer that can be added to a twig object. The event layer defines the possible outcomes of an event, their probabilities, and the transitions between states.
#'
#' @param name A character string representing the name of the event. It doesn't need to be quoted.
#' @param options A character vector of possible outcomes for the event. They don't need to be included in quotes. One of these options must be none.
#' @param probs A character vector of probability function names for each outcome. They don't need to be included in quotes. One of these can be leftover for the remaining probability.
#' @param transitions A character vector of state transitions corresponding to each outcome. They don't need to be included in quotes. These could be event names or states if a states layer defined. 
#' One of these can be stay for the Markov state to remain the same. 
#' @return A list representing the event layer.
#' @export
#' @examples
#' #' # Adding the event layer to a twig object
#' twig_obj <- twig() + event(name = event_progress, 
#'                            options = c(yes, none), 
#'                            probs = c(pProgress, leftover), 
#'                            transitions = c(Severe, stay))
#' 
#' event_layer <- event(name = "event_progress", 
#'                      options = c("yes", "none"), 
#'                      probs = c(pProgress, leftover), 
#'                      transitions = c("Severe", "stay"))
#' 
event <- function(name, options, probs, transitions){

  name <- deparse(substitute(name))
  options <- sapply(substitute(options), deparse)
  probs <- sapply(substitute(probs), deparse)
  transitions <- sapply(substitute(transitions), deparse)

  options <- remove_quotes(options)  
  probs <- remove_quotes(probs)  
  transitions <- remove_quotes(transitions)  
  name <- remove_quotes(name) 

  list(type = "event", 
       event = name, 
       options = options, 
       probs = probs, 
       transitions = transitions
  )
}

#' Add decisions to a twig
#'
#' @param names decision names, a character vector of decision names.  They don't need to be included in quotes.
#'
#' @return a twig layer with decision names
#' @export
#'
#' @examples 
#' decisions(names = c(A, B, C))
#' decisions(names = c("A", "B", "C"))
#' 
decisions <- function(names){
  names <- sapply(substitute(names), deparse)

  names <- remove_quotes(names)  # Remove leading and trailing quotes

  list(type = "decisions", decisions = names)
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
#' @param names ... a character vector of Markov state names. They don't need to be included in quotes.
#' @param init_probs ... a vector of initial probs, these could be numeric or function names. 
#' The functions can depend on the decision and variables in the params list of dataframe. 
#' One of these can be leftover for the remaining probability in that event.
#' init_probs must have the same length as state names.
#' @param max_cycles ... optional max tunnel lenghts (tunnel length). This defines the duration allowable in each state. 
#' If ignored a length of 1 is assumed. #' max_cycles if provided must have the same length as state names.
#' 
#' @return a twig layer with Markov state names
#' @export
#'
#' @examples states(names = c(H,S,D), 
#'                  init_probs = c(0.5, prob_fun, leftover),
#'                  max_cycles = c(1, 2, 1)) 
states <- function(names, init_probs, max_cycles = NULL) {
  names <- sapply(substitute(names), deparse)

  names <- remove_quotes(names)  

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

#' Add payoffs to a twig object
#'
#' This function creates a payoffs layer that can be added to a twig object. The payoffs layer defines the payoffs and their associated discount rates.
#'
#' @param names A character vector of payoff function names.  They don't need to be included in quotes.
#' @param discount_rates A numeric vector of discount rates for each payoff. If NULL, a discount rate of 0 is assumed for each payoff.
#' discount_rates must have the same length as payoff names.
#' @return A list representing the payoffs layer.
#' @export
#' @examples
#' payoffs_layer <- payoffs(names = c(cost, effectiveness), discount_rates = c(0.03, 0.03))
#' 
payoffs <- function(names, discount_rates=NULL){
  names <- sapply(substitute(names), deparse)

  names <- remove_quotes(names) 

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
  x <- gsub('^"|"$', '', x)  
  if (length(x)>1){
    x <- x[-1]
  }
  return(x)
}

