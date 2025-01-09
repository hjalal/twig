calculate_transition_probs <- function(P0_mat, A, dest_paths, unique_non_current_dest, dim_P, dimnames_P, is_cycle_dep, unique_dest_names, p_stay) {

    P_array <- P0_mat

    for (y in unique_non_current_dest) {

        sel_A <- A[, dest_paths[[y]], drop = FALSE]
        if (ncol(sel_A) > 1) {
            P_array[, y] <- rowSums(sel_A)
        } else {
            P_array[, y] <- sel_A
        }
    }

    dim(P_array) <- dim_P
    dimnames(P_array) <- dimnames_P
    if (is_cycle_dep) {
        P_array <- aperm(P_array, c(1, 4, 2, 3))
    } else {
        P_array <- aperm(P_array, c(1, 3, 2))
    }

    if ("stay" %in% unique_dest_names) {
        P_temp <- A[, dest_paths[["stay"]], drop = FALSE]
        if (ncol(P_temp) > 1) {
            P_temp <- rowSums(P_temp)
        } 
        P_array[p_stay] <- P_array[p_stay] + P_temp
    }

    return(P_array)
}
