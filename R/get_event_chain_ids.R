
build_lineage <- function(current_event, events_df) {

  current_row <- events_df[events_df$event == current_event, ]

  if (nrow(current_row) == 0) {
    return(list(current_row$event_id))
  }

  all_paths <- list()

  for (i in seq_len(nrow(current_row))) {
    child_event <- current_row$transitions[i]
    child_row <- events_df[events_df$event == child_event, ]

    if (nrow(child_row) > 0) {

      child_paths <- build_lineage(child_event, events_df)

      for (child_path in child_paths) {
        all_paths <- c(all_paths, list(c(current_row$event_id[i], child_path)))
      }
    } else {

      all_paths <- c(all_paths, list(c(current_row$event_id[i])))
    }
  }

  return(all_paths)
}

