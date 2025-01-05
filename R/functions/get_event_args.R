get_event_args <- function(twig_obj, all_args, all_events = FALSE){
  events <- retrieve_layer_by_type(twig_obj, type = "event")
  event_args <- unique(unlist(lapply(events, function(x) x$event)))
  if(!all_events){
    event_args <- event_args[event_args %in% all_args]
  }
  return(event_args)
}
