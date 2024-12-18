get_A <- function(A0_idx, E, A_idx, paths, n_paths) {
    # for each simulation sim, product of A
    # initialize a matrix [(D,S,C), path_id]
    
    A <- A0_idx
    
    # iterate through each path_id and multiply the E array by the event values along the paths
    for (path_id in 1:n_paths) {
        
        # get event ids on the path
        event_ids_on_path <- paths[[path_id]]
        for (i in seq_along(event_ids_on_path)) {
            
            # if it is the first event just get that value
            if (i == 1) {
                A[, path_id] <- E[A_idx[, path_id], event_ids_on_path[i]]
            } else {
                # multiply with the other event values along the path
                A[, path_id] <- A[, path_id] * E[A_idx[, path_id], event_ids_on_path[i]]
            }
        }
    }
    
    return(A)
}


