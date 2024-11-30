#' Builds a Markov model
#'
#' @param twig_env 
#'
#' @return a twig object containing the equations for the Markov model
#' @export
#' 
#' @examples 
#' print("see vignettes(package = 'twig')") 
twig_build <- function(x, ...) UseMethod("twig_build")

#' Build a Markov model structure
#'
#' @param twig_env 
#' @description takes in a twig formula structure and returns a twig numerical formula structure 
#' @return twig structure containing formulae strings
#' @export
#'
#' @examples 
#' print("see vignettes(package = 'twig')") 

twig_build.twig_markov <- function(twig_env, #n_cycles, 
                                   params = NULL, simplify = FALSE){
  # if (simplify){
  #   if (is.null(params)){
  #     stop("simplify = TRUE. please provide a list of paramters to simplify the generated model structure by removing paths that generate 0 probabilities. Avoid passing probabilities that are either 0 or 1.")
  #   } else { # not null params
  #   list2env(params, envir = .GlobalEnv)
  #   }
  # }
  
  # here we will have an environment to parse the twig_object
  #n_cycles <- twig_env$n_cycles

  #twig_env$n_cycles <- n_cycles
  twig_env$is_cycle_dep <- is_cycle_dep(twig_env)
  
  twig_env <- add_decision_info(twig_env)
  twig_env <- add_tunnels(twig_env)
  twig_env <- add_markov_info(twig_env)
  twig_env <- event_mapping_info(twig_env)
  twig_env <- add_markov_initial_probs(twig_env)
  events_df <- get_events_df(twig_env)
  twig_env$events <- unique(events_df$event)
  twig_env$n_events <- length(twig_env$events)
  #events_with_payoffs_df <- get_events_with_payoffs_df(events_df)
  twig_env$events_df <- events_df
  #twig_env$events_with_payoffs_df <- events_with_payoffs_df
  twig_env <- add_payoffs(twig_env)
  twig_env <- add_discounts_info(twig_env)
  twig_env <- add_markov_eqns(twig_env, events_df, simplify = simplify)
  #twig_env <- add_markov_payoff_eqns(twig_env, events_df)
  #twig_env <- add_event_prop_eqns(twig_env, events_df, events_with_payoffs_df)
  #twig_env <- add_markov_event_payoff_eqns(twig_env, events_df)
  
  class(twig_env) <- "twig_markov"
  return(twig_env)
  #print(twig_env)
}



#' Build a decision tree structure
#'
#' @param twig_env 
#' @description takes in a twig model syntax and returns a twig formula structure 
#' @return twig structure containing formulae strings
#' @export
#'
#' @examples 
#' print("see vignettes(package = 'twig')") 
twig_build.twig_decision <- function(twig_env, params = NULL, simplify = FALSE){
  if (simplify){
    if (is.null(params)){
      stop("simplify = TRUE. please provide a list of paramters to simplify the generated model structure by removing paths that generate 0 probabilities. Avoid passing probabilities that are either 0 or 1.")
    } else { # not null params
      list2env(params, envir = .GlobalEnv)
    }
  }
  # here we will have an environment to parse the twig_object
  #twig_env <- list()
  twig_env <- add_decision_info(twig_env)
  twig_env <- event_mapping_info(twig_env)
  twig_env <- add_final_outcome_info(twig_env)
  twig_env <- add_event_info(twig_env)
  twig_env <- add_payoffs(twig_env)
  # adding stirng equations 
  twig_env <- add_decision_eqns(twig_env, simplify = simplify)
  class(twig_env) <- "twig_decision"
  return(twig_env)
}

