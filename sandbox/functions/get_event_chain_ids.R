# Recursive function to build lineage paths using event_id
build_lineage <- function(current_event, events_df) {
  # Find the current row in the dataset
  current_row <- events_df[events_df$event == current_event, ]
  
  # If no children, return just the current event_id
  if (nrow(current_row) == 0) {
    return(list(current_row$event_id))
  }
  
  # Initialize a list to store all paths
  all_paths <- list()
  
  # Traverse each child
  for (i in seq_len(nrow(current_row))) {
    child_event <- current_row$goto[i]
    child_row <- events_df[events_df$event == child_event, ]
    
    if (nrow(child_row) > 0) {
      # Recursively build the lineage for the child
      child_paths <- build_lineage(child_event, events_df)
      
      # Append the current event_id to each child path
      for (child_path in child_paths) {
        all_paths <- c(all_paths, list(c(current_row$event_id[i], child_path)))
      }
    } else {
      # If no further descendants, include just the current and child event_ids
      all_paths <- c(all_paths, list(c(current_row$event_id[i])))
    }
  }
  
  return(all_paths)
}


