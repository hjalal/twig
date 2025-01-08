
retrieve_layer_by_type <- function(twig_obj, type) {
  
  outcome <- lapply(twig_obj$layers, function(x) if (x$type == type) x else NULL)
  
  lyr <- Filter(Negate(is.null), outcome)
  if (length(lyr) == 1 & type != "event") {
    lyr <- lyr[[1]] 
  }
  lyr
}