get_core_args <- function(twig_obj, all_args) {
  core_non_event_args <- get_core_non_event_args(all_args, twig_type = class(twig_obj)) 
  
  event_args <- get_event_args(twig_obj, all_args)
  core_args <- c(core_non_event_args, event_args)
  #core_args <- core_args[core_args %in% all_args]
  return(core_args)
}
