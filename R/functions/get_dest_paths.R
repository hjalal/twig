get_dest_paths <- function(paths, events_df, state_layer){
    state_names <- c(state_layer$names, "curr_state")

    # get dest names
    dest_names <- get_dest_names(paths, events_df, state_names)

    # get unique dest names
    unique_dest_names <- unique(dest_names)

    # add _tnl1 to tunnel states
    expand_dest_states <- expand_dest_state(unique_dest_names, state_layer)

    # get unique non current destinations
    
    # unique_non_current_dest <- expand_dest_states[expand_dest_states != "curr_state"]

    # get path_ids for each destination including the current state
    dest_paths <- get_dest_paths(expand_dest_states, paths, unique_dest_names, dest_names)
    return(dest_paths)
}