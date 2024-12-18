get_IDX_path_dep <- function(   A_idx, 
                                IDX_R, 
                                n_paths, 
                                n_event_dep_rewards, 
                                total_size_core_non_event_arguments, 
                                event_dep_rewards){
    # goal is to get a single matrix for each reward function:
    # dims = (S, C, D), k=path_id, event_dep_reward_id
    IDX_path_dep <- array(NA, dim = c(total_size_core_non_event_arguments, n_paths, n_event_dep_rewards), 
                        dimnames = list(NULL, path_id = NULL, event_dep_reward_id = event_dep_rewards))
    # get indices to move from the core arguments to the core non event arguments and paths
    # iterate through all the event values and find the corresponding indices in the E0 array
    # each path will have a unique set of indices that will retrieve the event values
    # along the path

    # just use A_idx which is a mapping from the core arguments(-sim) to core non event arguments and paths
    for (fun in event_dep_rewards){
        IDX_path_dep[,,fun] <- IDX_R[A_idx, fun]
    }
    return(IDX_path_dep)
}