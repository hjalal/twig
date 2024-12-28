run_twig <- function(twig_obj, params, n_cycles = NULL, verbose = FALSE, parallel = TRUE){
  
  if ("decision_twig" %in% class(twig_obj)) {
    # run model as a decision twig
    results <- run_decision_twig(twig_obj, params, verbose = verbose, parallel = parallel)

  } else if ( "markov_twig" %in% class(twig_obj)) {
    # add_markov_info(twig_obj)
    # add_markov_initial_probs(twig_obj)
    results <- run_markov_twig(twig_obj, params, n_cycles, verbose = verbose, parallel = parallel)
  
  } else {
    stop("twig object must be of class 'decision_twig' or 'markov_twig'")
  }
  return(results)

} # end of run_twig
