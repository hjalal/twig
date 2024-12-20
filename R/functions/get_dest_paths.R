get_dest_paths <- function(paths, events_df, state_layer, 
    dest_names, unique_dest_names, expand_dest_states){

    # get path_ids for each destination including the current state
    # dest_paths <- get_dest_paths(expand_dest_states, paths, unique_dest_names, dest_names)
    dest_paths <- list()
    for (i in seq_along(expand_dest_states) ){
        y <- expand_dest_states[i]
        dest <- unique_dest_names[i]
        dest_paths[[y]] <- which(dest_names == dest)
    }
    return(dest_paths)
}
