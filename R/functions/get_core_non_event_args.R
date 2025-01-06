get_core_non_event_args <- function(all_args, twig_type) {
  #
  if ("decision_twig" %in% twig_type){
    core_args <- c("decision", "outcome")
  } else if ("markov_twig" %in% twig_type){
    core_args <- c("state", "cycle", "decision")
  }
  core_args <- core_args[core_args %in% all_args]
  return(core_args)
}
