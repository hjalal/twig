







twig <- function() {
  twig_obj <- list() 
  class(twig_obj) <- c("decision_twig", "twig_class")
  return(twig_obj)
}














`+.twig_class` <- function(twig_obj, layer) {
  
  if (layer$type == "states") {
    
    message("Note: A states layer detected in your twig - treating Twig as a Markov model. 
            For a decision tree, make sure to remove the states layer.")
    class(twig_obj) <- NULL 
    class(twig_obj) <- c("markov_twig", "twig_class")
  }
  
  
  twig_obj$layers <- c(twig_obj$layers, list(layer))
  
  
  return(twig_obj)
}
























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













decisions <- function(names){
  names <- sapply(substitute(names), deparse)
  
  names <- remove_quotes(names)  
  
  list(type = "decisions", decisions = names)
}




to_strings <- function(expr_substituted) {
  if (is.call(expr_substituted)) {
    noquote(sapply(expr_substituted[-1], deparse))
  } else {
    noquote(deparse(expr_substituted))
  }
}

















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

  
  cycles_in_states <- unlist(sapply(max_cycles, seq_len))
  repeated_tunnels <- rep(max_cycles, max_cycles)
  
  repeated_states <- rep(names, max_cycles)
  tunneled_states <- ifelse(repeated_tunnels > 1, paste0(repeated_states, "_tnl", cycles_in_states), repeated_states)
  
  expanded_init_probs <- rep(0, length(cycles_in_states))
  expanded_init_probs[cycles_in_states == 1] <- init_probs

  
  cycles_in_states[repeated_tunnels == 1] <- NA
  l1 <- list(type = "states",
            names = names,
            init_probs = init_probs,
            max_cycles = max_cycles,
            expanded_init_probs = expanded_init_probs,
            cycles_in_states = cycles_in_states,
            
            repeated_states = repeated_states,
            tunneled_states = tunneled_states)
  
  return(l1)
}














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


