get_p0_funs <- function(twig_obj, type = NULL) {
  
  states <- retrieve_layer_by_type(twig_obj, type = "states")
  filter_functions(states$init_probs, is_prob_reward = FALSE)
}
