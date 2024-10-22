#' Builds a Markov model
#'
#' @param twig_obj 
#'
#' @return a twig object containing the equations for the Markov model
#' @export
#' 
#' @examples 
#' print("see vignettes(package = 'twig')") 
twig_build <- function(x, ...) UseMethod("twig_build")

#' Build a Markov model structure
#'
#' @param twig_obj 
#' @description takes in a twig formula structure and returns a twig numerical formula structure 
#' @return twig structure containing formulae strings
#' @export
#'
#' @examples 
#' print("see vignettes(package = 'twig')") 

twig_build.twig_markov <- function(twig_obj, #n_cycles, 
                                   params = NULL, simplify = FALSE){
  # if (simplify){
  #   if (is.null(params)){
  #     stop("simplify = TRUE. please provide a list of paramters to simplify the generated model structure by removing paths that generate 0 probabilities. Avoid passing probabilities that are either 0 or 1.")
  #   } else { # not null params
  #   list2env(params, envir = .GlobalEnv)
  #   }
  # }
  
  # here we will have an environment to parse the twig_object
  #n_cycles <- twig_obj$n_cycles
  model_obj <- list()
  #model_obj$n_cycles <- n_cycles
  model_obj$is_cycle_dep <- is_cycle_dep(twig_obj)
  
  model_obj <- add_decision_info(twig_obj, model_obj)
  model_obj <- add_tunnels(twig_obj, model_obj)
  model_obj <- add_markov_info(twig_obj, model_obj)
  model_obj <- event_mapping_info(twig_obj, model_obj)
  model_obj <- add_markov_initial_probs(twig_obj, model_obj)
  events_df <- get_event_df(twig_obj)
  model_obj$events <- unique(events_df$event)
  model_obj$n_events <- length(model_obj$events)
  #events_with_payoffs_df <- get_events_with_payoffs_df(events_df)
  model_obj$events_df <- events_df
  #model_obj$events_with_payoffs_df <- events_with_payoffs_df
  model_obj <- add_payoffs(twig_obj, model_obj)
  model_obj <- add_discounts_info(twig_obj, model_obj)
  model_obj <- add_markov_eqns(twig_obj, model_obj, events_df, simplify = simplify)
  #model_obj <- add_markov_payoff_eqns(twig_obj, model_obj, events_df)
  #model_obj <- add_event_prop_eqns(twig_obj, model_obj, events_df, events_with_payoffs_df)
  #model_obj <- add_markov_event_payoff_eqns(twig_obj, model_obj, events_df)
  
  class(model_obj) <- "twig_markov"
  return(model_obj)
  #print(model_obj)
}



#' Build a decision tree structure
#'
#' @param twig_obj 
#' @description takes in a twig model syntax and returns a twig formula structure 
#' @return twig structure containing formulae strings
#' @export
#'
#' @examples 
#' print("see vignettes(package = 'twig')") 
twig_build.twig_decision <- function(twig_obj, params = NULL, simplify = FALSE){
  if (simplify){
    if (is.null(params)){
      stop("simplify = TRUE. please provide a list of paramters to simplify the generated model structure by removing paths that generate 0 probabilities. Avoid passing probabilities that are either 0 or 1.")
    } else { # not null params
      list2env(params, envir = .GlobalEnv)
    }
  }
  # here we will have an environment to parse the twig_object
  model_obj <- list()
  model_obj <- add_decision_info(twig_obj, model_obj)
  model_obj <- event_mapping_info(twig_obj, model_obj)
  model_obj <- add_final_outcome_info(twig_obj, model_obj)
  model_obj <- add_event_info(twig_obj, model_obj)
  model_obj <- add_payoffs(twig_obj, model_obj)
  # adding stirng equations 
  model_obj <- add_decision_eqns(twig_obj, model_obj, simplify = simplify)
  class(model_obj) <- "twig_decision"
  return(model_obj)
}

