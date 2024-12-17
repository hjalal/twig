# Prep Transition probs:

# cross walk between dest and path_ids
paths
events_df
state_names <- c(state_layer$names, "curr_state")

# get dest names
dest_names <- get_dest_names(paths, events_df, state_names)

# get unique dest names
unique_dest_names <- unique(dest_names)

# add _tnl1 to tunnel states
expand_dest_states <- expand_dest_state(unique_dest_names, state_layer)

# get unique non current destinations
unique_non_current_dest <- expand_dest_states[expand_dest_states != "curr_state"]

# get path_ids for each destination including the current state
dest_paths <- get_dest_paths(expand_dest_states, paths, unique_dest_names, dest_names)

# get indices of staying in the current state
dim_P <- c(size_core_non_event_arguments, dest = n_expanded_states)
dimnames_P <- arg_values[core_non_event_args]
dimnames_P$dest <- expanded_states


p_stay <- get_stay_indices(state_layer, dim_P, dimnames_P, size_core_non_event_arguments, expanded_states, is_cycle_dep)


P0_mat <- matrix(0, nrow = prod(size_core_non_event_arguments), ncol = n_expanded_states)
colnames(P0_mat) <- expanded_states
P0_mat


# for each sim get the transition probs
#  Transition probs logic for each sim

