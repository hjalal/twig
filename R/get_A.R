get_A <- function(A0_idx, E, A_idx, paths, n_paths) {

    A <- A0_idx

    for (path_id in 1:n_paths) {

        event_ids_on_path <- paths[[path_id]]
        for (i in seq_along(event_ids_on_path)) {

            if (i == 1) {
                A[, path_id] <- E[A_idx[, path_id], event_ids_on_path[i]]
            } else {

                A[, path_id] <- A[, path_id] * E[A_idx[, path_id], event_ids_on_path[i]]
            }
        }
    }

    return(A)
}

