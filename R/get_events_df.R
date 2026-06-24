get_events_df <- function(twig_obj, hash_string = "leftover"){

  event_layers <- retrieve_layer_by_type(twig_obj, type = "event") 
  events_df_list <- list()
  i <- 0
  for (event_layer in event_layers){
    i <- i + 1
    temp_df <- as.data.frame(event_layers[[i]])

    temp_df$options <- as.character(temp_df$options)
    events_df_list[[i]] <- temp_df
  }
  events_df <- do.call(rbind, events_df_list)
  events_df$event_id <- seq_len(nrow(events_df))
  return(events_df)
}

get_compl_event_ids <- function(events_df, hash_string){
  events_df$id <- seq_len(nrow(events_df))

  hash_id <- events_df$id[events_df$probs == hash_string]
  compl_ids <- list()
  # Build one slot per 'leftover' row, keyed by that row's own event, in the
  # same order as hash_id (which get_E() consumes positionally). Iterating over
  # unique events instead mis-keyed slots whenever some event had no 'leftover'.
  for (h in hash_id){
    ev <- events_df$event[h]
    compl_ids[[ev]] <- events_df$id[events_df$event == ev & events_df$id != h]
  }
  return(compl_ids)
}

