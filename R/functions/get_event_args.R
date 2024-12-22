get_event_args <- function(twig_obj){
  events <- retrieve_layer_by_type(twig_obj, type = "event")
  event_args <- unique(unlist(lapply(events, function(x) x$event)))
  return(event_args)
}