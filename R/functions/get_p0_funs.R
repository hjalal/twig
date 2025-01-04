get_p0_funs <- function(twig_obj, type = NULL) {
  # get initial probs functions
  states <- retrieve_layer_by_type(twig_obj, type = "states")
  filter_functions(states$init_probs)
}
