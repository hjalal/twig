get_path_event_values <- function(n_paths, n_event_args, event_args, paths, events_df){
    

path_event_values <- matrix("none", nrow = n_paths, ncol = n_event_args, 
    dimnames = list(path_id = 1:n_paths, event_id = event_args))


for (path_id in 1:n_paths){
    event_ids_on_path <- paths[[path_id]]
    for (event_id in event_ids_on_path){
        event_data <- events_df[events_df$event_id == event_id, ]
        path_event_values[path_id, event_data$event] <- event_data$options
    }
}
return(path_event_values)
}