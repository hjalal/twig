#' Run a twig model
#'
#' This function runs a twig model, which can be either a decision tree or a Markov model.
#'
#' @param twig_obj A twig object created by the `twig` function.
#' @param params A data frame or list of parameters to be used in the model.
#' @param n_cycles An integer specifying the number of cycles for a Markov model. Default is NULL.
#' @param verbose A logical value indicating whether to print detailed output. Default is FALSE.
#' @param parallel A logical value indicating whether to run the model in parallel. Default is FALSE.
#' @param offset_trace_cycle An integer specifying the offset trace cycle. Default is 1. 
#' This is used to adjust the cycle number in the trace output. If set to 0, the initial state distribution will be used 
#' as the first cycle. If set to 1, the initial state distribution will be ignored in the Markov trace.
#' In both situations, the total number of cycles will be the same as the input n_cycles.
#' @param ncore An integer specifying the number of cores to use for parallel processing. Default is total number of cores - 1.
#' @return A list containing the results of the model run. The list includes the following elements:
#' \itemize{
#' \item{Rewards_summary}{An array containing the summary statistics of the rewards if params is a data.frame with more than 1 row.}
#' \item{Rewards_sim}{An array containing the simulated rewards by simulation.}
#' }
#' The following will be included if verbose is TRUE and the twig object is a Markov model:
#' \itemize{
#' \item{sim}{The simulation ID.}
#' \item{Rewards_sim}{A matrix of simulated rewards of size decision x rewards.}
#' \item{Rewards_array}{An array containing the rewards for each cycle, state, decision, and reward.}
#' \item{Rewards_array_cycle}{An array containing the rewards for each cycle, state, decision, and reward, adjusted for the cycle.}
#' \item{Trace_array}{An array containing the Markov trace for each cycle, state, and decision.}
#' \item{TransitionProb_array}{An array containing the transition probabilities for each state, destination state, cycle, and decision.}
#' \item{Paths}{A data frame containing the path probabilities for each state, cycle, and decision.}
#' \item{Event_options}{A data frame containing the event options for each state, cycle, and decision.}
#' \item{Function_Values}{A data frame containing the evaluated function values for each state, cycle, decision, and event.}
#' \item{evaluated_funs}{A list of evaluated functions. Each function returns a data frame enumerating the dependencies of the function along with the value returned by that function for each combination of values.}
#' \item{path_events}{A data frame of the event options along each path. Rows = paths, columns = events.}
#' }
#' The following will be included if verbose is TRUE and the twig object is a decision tree:
#' \itemize{
#' \item{sim}{The simulation ID.}
#' \item{Rewards_sim}{A matrix of simulated rewards of size decision x rewards.}
#' \item{Path_rewards}{An array containing the path rewards of size decision x paths x rewards. Paths are indexed by their final outcomes in the twig and a key to event options is provided in path_events.}
#' \item{Path_rewards_weighted}{An array containing the path rewards weighted by the probability of each path. This is also of size decision x paths x rewards. Paths are indexed by their final outcomes in the twig and a key to event options is provided in path_events.}
#' \item{Paths}{An array containing the path probabilities of size decision x paths.}
#' \item{Outcomes}{A matrix of final outcome probabilities. Final outcomes are the terminal event transitions of size decision x outcomes.}
#' \item{Event_options}{A data frame containing the probability of event options by decision.}
#' \item{path_events}{A data frame of the event options along each path. Rows = paths, columns = events.}
#' \item{evaluated_funs}{A list of evaluated functions. Each function returns a data frame enumerating the dependencies of the function along with the value returned by that function for each combination of values.}
#' \item{Prob_Function_Values}{A data frame of the probability function values evaluated by decision and events harmonized to the same combinations of decisions and events across all probability functions.}
#' }
#' @seealso
#' \href{../doc/markov_time_dep.html}{Markov model vignette}, and 
#' \href{../doc/decision_tree.html}{Decision tree vignette}
#' @export
#' @examples
#' \dontrun{
#' # Create a twig object
#' twig_obj <- twig() + 
#'   decisions("StandardOfCare", "StrategyA") + 
#'   states(names = c("H", "S", "D"), init_probs = c(1,0,0)) + 
#'   event(name = "only_event",  
#'         options = c("get_sick", "die", "stay"), 
#'         probs = c(pGetSick(state), pDie(decision, state), Inf), 
#'         transitions = c("S", "D", "stay")) +  
#'   payoffs(cost = compute_cost(decision, state))
#'
#' # Define parameters
#' params <- data.frame(
#'   pGetSick = 0.1,
#'   pDie = 0.05,
#'   compute_cost = function(decision, state) { return(100) }
#' )
#'
#' # Run the twig model
#' results <- run_twig(twig_obj, params, n_cycles = 10, verbose = TRUE, parallel = FALSE)
#' }
#' 
run_twig <- function(twig_obj, params, n_cycles = NULL, verbose = FALSE, parallel = FALSE, offset_trace_cycle = 1, ncore = NULL){
   hash_string <- "leftover" 

   # check twig syntax
   check_twig(twig_obj)

  if ("decision_twig" %in% class(twig_obj)) {

    # run model as a decision twig
    results <- run_decision_twig(twig_obj, params, verbose = verbose, parallel = parallel, hash_string, ncore = ncore)

  } else if ( "markov_twig" %in% class(twig_obj)) {
    # run model as a markov twig
    results <- run_markov_twig(twig_obj, params, n_cycles, verbose = verbose, 
                               parallel = parallel, hash_string, offset_trace_cycle = offset_trace_cycle, 
                               ncore = ncore)

  } else {
    stop("twig object must be of class 'decision_twig' or 'markov_twig'")
  }
  return(results)

} # end of run_twig
