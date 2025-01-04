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

twig_build.markov_twig <- function(twig_obj){
  # if (simplify){
  #   if (is.null(params)){
  #     stop("simplify = TRUE. please provide a list of paramters to simplify the generated model structure by removing paths that generate 0 probs. Avoid passing probs that are either 0 or 1.")
  #   } else { # not null params
  #   list2env(params, envir = .GlobalEnv)
  #   }
  # }
  
  # here we will have an environment to parse the twig_object
  #n_cycles <- twig_obj$n_cycles

  #twig_obj$n_cycles <- n_cycles
  twig_obj$is_cycle_dep <- is_cycle_dep(twig_obj)
  
  #add_decision_info(twig_obj)
  #add_tunnels(twig_obj)
  #add_markov_info(twig_obj)
  #event_mapping_info(twig_obj)
  #add_markov_initial_probs(twig_obj)
  get_events_df(twig_obj)

  #events_with_payoffs_df <- get_events_with_payoffs_df(events_df)
  #twig_obj$events_with_payoffs_df <- events_with_payoffs_df
  add_payoffs(twig_obj)
  #add_markov_eqns(twig_obj)
  #twig_obj <- add_markov_payoff_eqns(twig_obj, events_df)
  #twig_obj <- add_event_prop_eqns(twig_obj, events_df, events_with_payoffs_df)
  #twig_obj <- add_markov_event_payoff_eqns(twig_obj, events_df)
  
  #class(twig_obj) <- "markov_twig"
  #return(twig_obj)
  #print(twig_obj)
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
twig_build.decision_twig <- function(twig_obj){
  # if (simplify){
  #   if (is.null(params)){
  #     stop("simplify = TRUE. please provide a list of paramters to simplify the generated model structure by removing paths that generate 0 probs. Avoid passing probs that are either 0 or 1.")
  #   } else { # not null params
  #     list2env(params, envir = .GlobalEnv)
  #   }
  # }
  # here we will have an environment to parse the twig_object
  #twig_obj <- list()
  #add_decision_info(twig_obj)
  #event_mapping_info(twig_obj)
  add_final_outcome_info(twig_obj)
  # add_event_info(twig_obj)
  add_payoffs(twig_obj)
  # adding string equations 
  add_decision_eqns(twig_obj)
  #class(twig_obj) <- "decision_twig"
  #return(twig_obj)
}

