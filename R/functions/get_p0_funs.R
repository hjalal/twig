get_p0_funs <- function(twig_obj, type = NULL) {
  # get initial probabilities functions
  states <- retrieve_layer_by_type(twig_obj, type = "states")
  filter_functions(states$initial_probabilities)
}
