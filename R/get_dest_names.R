



































get_dest_names <- function(paths, events_df, state_names){
dest_names <- rep(NA, length(paths))



for (path_id in seq_along(paths)){
    
    event_ids <- paths[[path_id]]
    
    transitions <- events_df$transitions[events_df$event_id %in% event_ids]
    
    dest_names[path_id] <- transitions[transitions %in% state_names]
}
return(dest_names)
}












