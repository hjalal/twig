# retrieves Twig layers by their type.
retrieve_layer_by_type <- function(twig_obj, type) {
  # Use lapply to filter the list based on the condition
  outcome <- lapply(twig_obj$layers, function(x) if (x$type == type) x else NULL)
  # Remove NULL elements from the list
  lyr <- Filter(Negate(is.null), outcome)
  if (length(lyr) == 1 & type != "event") {
    lyr <- lyr[[1]] # only select the first element if there is no more
  }
  lyr
}