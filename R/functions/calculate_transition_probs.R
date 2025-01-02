calculate_transition_probs <- function(P0_mat, A, dest_paths, unique_non_current_dest, dim_P, dimnames_P, is_cycle_dep, unique_dest_names, p_stay) {
    # Transition probabilities logic for each sim
    # initialize P0_mat[(S,C,D), Y] = 0
    P_array <- P0_mat

    # iterate through non_current_dest destinations 
    # and for each destination add the probabilities of the paths that lead to that destination
    for (y in unique_non_current_dest) {
    # browser()
        sel_A <- A[, dest_paths[[y]], drop = FALSE]
        if (ncol(sel_A) > 1) {
            P_array[, y] <- rowSums(sel_A)
        } else {
            P_array[, y] <- sel_A
        }
    }

    # reshape P_array to [S,Y,C,D] 
    # important because the indexing from the curr_state is vectorized.
    # so it is important for S and Y to be the first initial states.
    dim(P_array) <- dim_P
    dimnames(P_array) <- dimnames_P
    if (is_cycle_dep) {
        P_array <- aperm(P_array, c(1, 4, 2, 3))
    } else {
        P_array <- aperm(P_array, c(1, 3, 2))
    }

    # add staying probabilities curr_state
    # first sum across all those that point to the current state
    # then add it to the existing probabilities using the 
    # predefined indices p_stay.
    if ("curr_state" %in% unique_dest_names) {
        P_temp <- A[, dest_paths[["curr_state"]], drop = FALSE]
        if (ncol(P_temp) > 1) {
            P_temp <- rowSums(P_temp)
        } 
        P_array[p_stay] <- P_array[p_stay] + P_temp
    }

    # # check to make sure they all add up to 1
    # if (any(apply(P_array, c(1, 3, 4), sum) != 1)) {
    #     warning("Transition probabilities do not sum to 1.")
    # }

    return(P_array)
}
