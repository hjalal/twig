get_core_non_event_args <- function(all_args) {
  core_args <- c("state", "cycle", "decision", "outcome")
  core_args <- core_args[core_args %in% all_args]
  return(core_args)
}
