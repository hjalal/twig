get_core_non_event_args <- function(all_args, twig_type) {

  if ("decision_twig" %in% twig_type){
    core_args <- c("decision", "outcome")
    core_args <- core_args[core_args %in% c("decision", all_args)]
  } else if ("markov_twig" %in% twig_type){
    core_args <- c("state", "cycle", "decision")
    # state and decision are intrinsic dimensions of a Markov model and are kept
    # even when no function references them; cycle is kept only when used.
    core_args <- core_args[core_args %in% c("state", "decision", all_args)]
  }
  return(core_args)
}
