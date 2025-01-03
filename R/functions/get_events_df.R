get_events_df <- function(twig_obj, hash_string = "leftover"){
      # hash_string <- "\"#\""
  event_layers <- retrieve_layer_by_type(twig_obj, type = "event") 
  events_df_list <- list()
  i <- 0
  for (event_layer in event_layers){
    i <- i + 1
    temp_df <- as.data.frame(event_layers[[i]])
    # temp_df$probabilities[temp_df$probabilities == hash_string] <- "COMPLEMENT"
    temp_df$options <- as.character(temp_df$options)
    events_df_list[[i]] <- temp_df
  }
  events_df <- do.call(rbind, events_df_list)
  events_df$event_id <- seq_len(nrow(events_df))
  return(events_df)
}

get_compl_event_ids <- function(events_df, hash_string){
  events_df$id <- seq_len(nrow(events_df))
  # return a list of event_ids and their complements
  # hash_string <- "COMPLEMENT" #"\"#\""
  
  twig_obj$hash_id <- hash_id <- events_df$id[events_df$probabilities == hash_string]
  compl_ids <- list()
  unique_events <- unique(events_df$event)
  for (i in 1:length(unique_events)){
    
    compl_ids[[unique_events[i]]] <- events_df$id[events_df$event == events_df$event[hash_id[i]] & 
    events_df$id != events_df$id[hash_id[i]]]
  }
  return(compl_ids)
}

  
