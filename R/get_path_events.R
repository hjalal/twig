
get_path_events <- function(paths, events_df, n_paths, all_event_args, dest_paths){
        
    
    dest <- rep(NA, n_paths)
    for (i in 1:n_paths){
        dest[i] <- names(dest_paths)[sapply(dest_paths, function(x) i %in% x)]
    }
    


    path_events <- data.frame(paths = 1:n_paths, dest = dest)
    for (event in all_event_args){
        path_events[[event]] <- "none"
    }

    for (i in 1:n_paths){
        path <- paths[[i]]
        for (event in path){
            event_data <- events_df[events_df$event_id == event, ]
            path_events[i, event_data$event] <- event_data$options
        }
    }
    return(path_events)
}

