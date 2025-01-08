get_E <- function(E0, F, non_compl_id, event_prob_link, hash_id, compl_id) {

    E <- E0

    E[, non_compl_id] <- F[, event_prob_link[non_compl_id]]

    for (i in 1:length(hash_id)) {
        E[, hash_id[i]] <- 1 - rowSums(E[, compl_id[[i]], drop = FALSE])
    }

    return(E)
}
