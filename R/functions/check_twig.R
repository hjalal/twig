# Check if layers exist and is a non-empty list
check_layers <- function(twig_obj) {
  if (is.null(twig_obj$layers) || length(twig_obj$layers) == 0) {
    stop("Error: 'layers' is missing or empty in twig_obj.")
  }
}

# Check if each layer has the correct 'type' and expected structure
check_layer_types <- function(twig_obj) {
  expected_types <- c("decisions", "states", "event", "payoffs")
  
  layer_types <- sapply(twig_obj$layers, function(layer) layer$type)
  
  # Check if there are multiple of the same layer type (except 'event')
  for (layer_type in expected_types) {
    if (layer_type != "event" && sum(layer_types == layer_type) > 1) {
      stop(paste("Error: More than one", layer_type, "layer found."))
    }
  }
  
  # Allow multiple 'event' layers but ensure there is at least one
  if (sum(layer_types == "event") < 1) {
    stop("Error: At least one 'event' layer must be present.")
  }
}


# Check if each event option has exactly one "none"
check_none_in_events <- function(twig_obj) {
  for (layer in twig_obj$layers) {
    if (layer$type == "event" && "options" %in% names(layer)) {
      none_count <- sum(layer$options == "none")
      if (none_count != 1) {
        stop(paste("Error: Event '", layer$event, "' must have exactly one 'none' value. Found:", none_count))
      }
    }
  }
}

# Check if all names across events, decisions, and states are unique
check_unique_names <- function(twig_obj) {
  # Initialize an empty vector to collect all names
  all_names <- c()
  
  for (layer in twig_obj$layers) {
    if (layer$type == "decisions") {
      # Add decision names
      all_names <- c(all_names, layer$decisions)
    } else if (layer$type == "states") {
      # Add state names
      all_names <- c(all_names, layer$names)
    } else if (layer$type == "event") {
      # Add event name
      all_names <- c(all_names, layer$event)
    }
  }
  
  # Find duplicates
  duplicate_names <- all_names[duplicated(all_names)]
  
  if (length(duplicate_names) > 0) {
    stop(paste("Error: Duplicate names found across events, decisions, or states:", 
               paste(unique(duplicate_names), collapse = ", ")))
  }
}


check_event_transitions_valid <- function(twig_obj) {
  # Get state names if a states layer exists
  state_layer <- twig_obj$layers[sapply(twig_obj$layers, function(layer) layer$type == "states")]
  state_names <- if (length(state_layer) > 0) state_layer[[1]]$names else NULL
  
  # Collect all event names
  event_names <- unique(unlist(lapply(twig_obj$layers, function(layer) {
    if (layer$type == "event") return(layer$event)
    return(NULL)
  })))
  if (length(state_layer) > 0){
    
    # Check transitions in event layers
    for (layer in twig_obj$layers) {
      if (layer$type == "event" && "transitions" %in% names(layer)) {
        # Valid transitions include state names, event names, and "current_state"
        valid_transitions <- c(state_names, event_names, "current_state")
        invalid_transitions <- setdiff(layer$transitions, valid_transitions)
        
        if (length(invalid_transitions) > 0) {
          stop(
            paste0(
              "Error: Invalid transitions in event '", 
              layer$event, 
              "': ", 
              paste(invalid_transitions, collapse = ", ")
            )
          )
        }
      }
  } 
  
  } else {
  
  # If there is no states layer, process transitions differently
  # if (is.null(state_names)) {
    # Separate event transitions into event and non-event (final outcomes)
    all_transitions <- unlist(lapply(twig_obj$layers, function(layer) {
      if (layer$type == "event" && "transitions" %in% names(layer)) {
        return(layer$transitions)
      }
      return(NULL)
    }))
    
    event_transitions <- all_transitions[all_transitions %in% event_names]
    non_event_transitions <- setdiff(all_transitions, event_transitions)
    
    # Warning message
    warning_message <- paste(
      "A states layer was not detected.",
      "Event transitions include the following event names: ",
      paste(event_transitions, collapse = ", "),
      ". These are valid.",
      "\nThe following transitions are not event names and will be treated as final outcomes: ",
      paste(non_event_transitions, collapse = ", "),
      "."
    )
    message(warning_message)
  }
 # }
  
}


# Warn if there are states not included as event transitions
warn_unused_states <- function(twig_obj) {
  state_names <- twig_obj$layers[[2]]$names
  used_states <- unlist(sapply(twig_obj$layers, function(layer) if ("transitions" %in% names(layer)) layer$transitions else NULL))
  
  unused_states <- setdiff(state_names, used_states)
  if (length(unused_states) > 0) {
    warning("Warning: The following states are not included as event transitions: ", paste(unused_states, collapse = ", "))
  }
}

# 
check_leftover_in_states <- function(twig_obj) {
  # Find the states layer
  states_layer <- twig_obj$layers[sapply(twig_obj$layers, function(layer) layer$type == "states")]
  if (length(states_layer) == 0) {
    return()
  }
  # Ensure there is only one states layer
  if (length(states_layer) > 1) {
    stop("Error: There should at most one states layer.")
  }
  
  # Extract initial_probabilities from the states layer
  initial_probabilities <- states_layer[[1]]$initial_probabilities
  
  # Count occurrences of "leftover"
  leftover_count <- sum(initial_probabilities == "leftover")
  
  # Check if there is more than one "leftover"
  if (leftover_count > 1) {
    stop("Error: The states layer can have at most one 'leftover' in initial_probabilities.")
  }
}

check_leftover_in_events <- function(twig_obj) {
  # Find all event layers
  event_layers <- twig_obj$layers[sapply(twig_obj$layers, function(layer) layer$type == "event")]
  
  # Iterate through each event layer
  for (i in seq_along(event_layers)) {
    probabilities <- event_layers[[i]]$probabilities
    
    # Count occurrences of "leftover" in probabilities
    leftover_count <- sum(probabilities == "leftover")
    
    # Check if there is more than one "leftover"
    if (leftover_count > 1) {
      stop(sprintf("Error: Event layer %d has more than one 'leftover' in probabilities.", i))
    }
  }
}




# Ensure there is only one layer of each type (but allow multiple event layers)
check_single_layer_types <- function(twig_obj) {
  layer_types <- sapply(twig_obj$layers, function(layer) layer$type)
  
  if (sum(layer_types == "decisions") > 1) {
    stop("Error: More than one 'decisions' layer found.")
  }
  
  if (sum(layer_types == "decisions") < 1) {
    stop("Error: A single 'decisions' layer is required. 
         If you have a single decision or strategy, make the layer of that decision.
         (e.g., decisions(StandardOfCare))")
  }
  
  if (sum(layer_types == "states") > 1) {
    stop("Error: More than one 'states' layer found.")
  }
  
  if (sum(layer_types == "payoffs") > 1) {
    stop("Error: More than one 'payoffs' layer found.")
  }
  
  if (sum(layer_types == "payoffs") < 1) {
    stop("Error: A single 'payoffs' layer is required. 
         If you have a single payoff, make the layer of that payoff.
         (e.g., payoffs(names = cost))")
  }
}

# Helper function to check if a string is valid
valid_string <- function(x) {
  grepl("^[A-Za-z][A-Za-z0-9_]*$", x)
}

# Helper function to check if a value is numeric or convertible to numeric
valid_numeric_or_string <- function(x) {
  if (is.character(x)) {
    return(!is.na(suppressWarnings(as.numeric(x))) || valid_string(x))
  }
  return(FALSE)
}

# Helper function to check if a value is a strictly numeric string
valid_discount <- function(x) {
  is.numeric(x) || (is.character(x) && grepl("^[0-9.]+$", x))
}

# Check decision names
check_decisions <- function(decisions) {
  invalid_decisions <- decisions[!sapply(decisions, valid_string)]
  if (length(invalid_decisions) > 0) {
    stop("Error: Invalid decision names found: ", paste(invalid_decisions, collapse = ", "))
  }
}

# Check state names
check_state_names <- function(state_names) {
  invalid_state_names <- state_names[!sapply(state_names, valid_string)]
  if (length(invalid_state_names) > 0) {
    stop("Error: Invalid state names found: ", paste(invalid_state_names, collapse = ", "))
  }
}

# Check initial_probabilities for states
check_init_probs <- function(initial_probabilities) {
  invalid_init_probs <- initial_probabilities[!sapply(initial_probabilities, valid_numeric_or_string)]
  if (length(invalid_init_probs) > 0) {
    stop("Error: Invalid initial probabilities found: ", paste(invalid_init_probs, collapse = ", "))
  }
}

# Check event names
check_event_names <- function(event_names) {
  invalid_event_names <- event_names[!sapply(event_names, valid_string)]
  if (length(invalid_event_names) > 0) {
    stop("Error: Invalid event names found: ", paste(invalid_event_names, collapse = ", "))
  }
}

# Check probabilities in events
check_event_probabilities <- function(probabilities) {
  invalid_event_probs <- probabilities[!sapply(probabilities, valid_numeric_or_string)]
  if (length(invalid_event_probs) > 0) {
    stop("Error: Invalid event probabilities found: ", paste(invalid_event_probs, collapse = ", "))
  }
}

# Check transitions in events
check_event_transitions <- function(transitions) {
  invalid_event_transitions <- transitions[!sapply(transitions, valid_string)]
  if (length(invalid_event_transitions) > 0) {
    stop("Error: Invalid event transitions found: ", paste(invalid_event_transitions, collapse = ", "))
  }
}

# Check payoff names
check_payoff_names <- function(payoff_names) {
  invalid_payoff_names <- payoff_names[!sapply(payoff_names, valid_string)]
  if (length(invalid_payoff_names) > 0) {
    stop("Error: Invalid payoff names found: ", paste(invalid_payoff_names, collapse = ", "))
  }
}

# Check discount rates in payoffs
check_payoff_discount_rates <- function(discount_rates) {
  invalid_discounts <- discount_rates[!sapply(discount_rates, valid_discount)]
  if (length(invalid_discounts) > 0) {
    stop("Error: Invalid discount rates found: ", paste(invalid_discounts, collapse = ", "))
  }
}

apply_checks <- function(twig_obj) {
  for (layer in twig_obj$layers) {
    if (layer$type == "decisions" && "decisions" %in% names(layer)) {
      check_decisions(layer$decisions)
    }
    if (layer$type == "states") {
      if ("names" %in% names(layer)) check_state_names(layer$names)
      if ("initial_probabilities" %in% names(layer)) check_init_probs(layer$initial_probabilities)
    }
    if (layer$type == "event") {
      if ("event" %in% names(layer)) check_event_names(layer$event)
      if ("probabilities" %in% names(layer)) check_event_probabilities(layer$probabilities)
      if ("transitions" %in% names(layer))  check_event_transitions(layer$transitions)
    }
    if (layer$type == "payoffs") {
      if ("payoffs" %in% names(layer)) check_payoff_names(layer$payoffs)
      if ("discount_rates" %in% names(layer)) check_payoff_discount_rates(layer$discount_rates)
    }
  }
}

validate_twig_obj <- function(twig_obj) {
  # Helper Functions
  check_decision_count <- function(decisions) {
    if (length(decisions) == 0) {
      stop("Error: Decisions layer must have at least one decision.")
    }
  }
  
  check_state_consistency <- function(state_names, initial_probabilities, max_cycle_in_states = NULL) {
    if (length(state_names) != length(initial_probabilities)) {
      stop("Error: The number of state names does not match the number of initial probabilities.")
    }
    if (!is.null(max_cycle_in_states) && length(state_names) != length(max_cycle_in_states)) {
      stop("Error: The number of state names does not match the number of tunnel lengths.")
    }
  }
  
  check_event_consistency <- function(event_name, probabilities, transitions, options) {
    if (length(event_name) != 1) {
      stop("Error: Each event must have a single name.")
    }
    if (length(probabilities) != length(transitions) || length(transitions) != length(options)) {
      stop("Error: The number of probabilities, transitions, and options in an event must be equal.")
    }
  }
  
  check_payoff_consistency <- function(payoff_names, discount_rates = NULL) {
    if (!is.null(discount_rates) && length(payoff_names) != length(discount_rates)) {
      stop("Error: The number of payoff names does not match the number of discount rates.")
    }
  }
  
  # Validation Logic
  for (layer in twig_obj$layers) {
    if (layer$type == "decisions" && "decisions" %in% names(layer)) {
      check_decision_count(layer$decisions)
    }
    if (layer$type == "states") {
      if ("names" %in% names(layer) && "initial_probabilities" %in% names(layer)) {
        check_state_consistency(layer$names, layer$initial_probabilities, layer$max_cycle_in_states)
      }
    }
    if (layer$type == "event") {
      if ("event" %in% names(layer) && "probabilities" %in% names(layer) &&
          "transitions" %in% names(layer) && "options" %in% names(layer)) {
        check_event_consistency(layer$event, layer$probabilities, layer$transitions, layer$options)
      }
    }
    if (layer$type == "payoffs") {
      if ("payoffs" %in% names(layer)) {
        check_payoff_consistency(layer$payoffs, layer$discount_rates)
      }
    }
  }
  
}

check_tunnel_lengths <- function(twig_obj) {
  # Find the states layer
  state_layer <- twig_obj$layers[sapply(twig_obj$layers, function(layer) layer$type == "states")]
  
  # If a states layer exists and includes max_cycle_in_states
  if (length(state_layer) > 0 && "max_cycle_in_states" %in% names(state_layer[[1]])) {
    max_cycle_in_states <- state_layer[[1]]$max_cycle_in_states
    
    # Check that all max_cycle_in_states are integers > 0
    if (!all(sapply(max_cycle_in_states, function(x) is.numeric(x) && x == as.integer(x) && x > 0))) {
      stop("Error: All max_cycle_in_states in the states layer must be integers greater than 0.")
    }
  }
}

# Check if each event has more than 1 option
check_event_options_length <- function(twig_obj) {
  for (layer in twig_obj$layers) {
    if (layer$type == "event" && "options" %in% names(layer)) {
      if (length(layer$options) <= 1) {
        stop(paste("Error: Event '", layer$event, "' must have more than 1 option."))
      }
    }
  }
}

# Check if each event has unique options
check_event_options_unique <- function(twig_obj) {
  for (layer in twig_obj$layers) {
    if (layer$type == "event" && "options" %in% names(layer)) {
      if (length(layer$options) <= 1) {
        stop(paste("Error: Event '", layer$event, "' must have more than 1 option."))
      }
      if (length(layer$options) != length(unique(layer$options))) {
        stop(paste("Error: Event '", layer$event, "' must have unique options."))
      }
    }
  }
}

# Run all checks
check_twig <- function(twig_obj) {
  message("Checking Twig syntax .... ")
  check_layers(twig_obj)
  check_layer_types(twig_obj)
  check_none_in_events(twig_obj)
  check_unique_names(twig_obj)
  check_event_transitions_valid(twig_obj)
  warn_unused_states(twig_obj)
  check_leftover_in_states(twig_obj)
  check_leftover_in_events(twig_obj)
  check_single_layer_types(twig_obj)
  apply_checks(twig_obj)
  validate_twig_obj(twig_obj)
  check_tunnel_lengths(twig_obj)
  check_event_options_length(twig_obj)
  check_event_options_unique(twig_obj)
  message("Twig syntax validation completed successfully.")
}

