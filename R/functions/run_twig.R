run_twig <- function(twig_obj, params, n_cycles = NULL, verbose = FALSE, parallel = TRUE, offset_trace_cycle = 1){
   hash_string <- "leftover" # "\"#\""
  
  if ("decision_twig" %in% class(twig_obj)) {

    # run model as a decision twig
    results <- run_decision_twig(twig_obj, params, verbose = verbose, parallel = parallel, hash_string)

  } else if ( "markov_twig" %in% class(twig_obj)) {
    # add_markov_info(twig_obj)
    # add_markov_initial_probs(twig_obj)
    results <- run_markov_twig(twig_obj, params, n_cycles, verbose = verbose, parallel = parallel, hash_string, offset_trace_cycle = offset_trace_cycle)
  
  } else {
    stop("twig object must be of class 'decision_twig' or 'markov_twig'")
  }
  return(results)

} # end of run_twig
