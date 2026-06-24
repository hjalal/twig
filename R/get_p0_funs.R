get_p0_funs <- function(twig_obj, type = NULL, envir = parent.frame()) {

  states <- retrieve_layer_by_type(twig_obj, type = "states")
  filter_functions(states$init_probs, is_prob_payoff = FALSE, envir = envir)
}
