get_prob_funs <- function(twig_obj) {
  # Get events from twig object
  events <- retrieve_layer_by_type(twig_obj, type = "event")

  # Extract and flatten all probability values
  all_probs <- unlist(lapply(events, function(x) x$probs))

  # Remove '#' placeholder and get unique values
  unique_probs <- unique(all_probs[!all_probs %in% c('leftover', "complement")])

  filter_functions(unique_probs)
}
