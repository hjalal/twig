# creates a dataset of paths, destination and event values as columns.
get_path_events <- function(paths, events_df, n_paths, event_args, dest_paths){
        # Given single content_id
    
    dest <- rep(NA, n_paths)
    for (i in 1:n_paths){
        dest[i] <- names(dest_paths)[sapply(dest_paths, function(x) i %in% x)]
    }
    # Find the name of the list item containing the content_id


    path_events <- data.frame(paths = 1:n_paths, dest = dest)
    for (event in event_args){
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

