get_E <- function(E0, F, non_compl_id, event_prob_link, hash_id, compl_id) {
    # Initialize E with E0
    E <- E0
    
    # Update E for non-compliant IDs using F and event_prob_link
    E[, non_compl_id] <- F[, event_prob_link[non_compl_id]]
    
    # Update E for hash IDs by calculating 1 - rowSums of compliant IDs
    for (i in 1:length(hash_id)) {
        E[, hash_id[i]] <- 1 - rowSums(E[, compl_id[[i]], drop = FALSE])
    }
    
    # Return the updated E
    return(E)
}
