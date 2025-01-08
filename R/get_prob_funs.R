get_prob_funs <- function(twig_obj) {

  events <- retrieve_layer_by_type(twig_obj, type = "event")

  all_probs <- unlist(lapply(events, function(x) x$probs))

  unique_probs <- unique(all_probs[!all_probs %in% c('leftover', "complement")])

  filter_functions(unique_probs)
}
